;;; sops-file.el --- Transparently manipulate sops encrypted files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ahmad Jarara <ajarara@fastmail.com>

;; Author:  Ahmad Jarara <ajarara@fastmail.com>
;; Keywords: convenience, programming
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/ajarara/sops-file.el
;; Keywords: convenience files tools sops encrypt decrypt

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides for manipulating the content of sops files transparently.

;;; Code:
(require 'cl-lib)

(defgroup sops-file nil
  "Transparently manipulate SOPS files"
  :prefix 'sops-file
  :group 'convenience)

(defcustom sops-file-executable
  "sops"
  "Path to the sops executable."
  :group 'sops-file
  :type 'string)

(defcustom sops-file-decrypt-args
  `("decrypt")
  "Decrypt arguments for sops."
  :group 'sops-file
  :type '(repeat string))

(defcustom sops-file-encrypt-args
  `("encrypt")
  "Encrypt arguments for sops."
  :group 'sops-file
  :type '(repeat string))

(defcustom sops-file-mode-inferrer
  (lambda ()
    (let ((buffer-file-name
           (string-replace ".enc" "" buffer-file-name)))
      (normal-mode)))
  "Manipulate the mode of the file after decoding it"
  :group 'sops-file
  :type 'function)

(defcustom sops-file-error-renderer
  (lambda (stderr-buf)
    (with-current-buffer (get-buffer-create "*sops-file-error*")
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert-buffer-substring stderr-buf))
      (special-mode)
      (message "Could not decrypt visited file, see *sops-file-error* for sops output")))
  "Report sops error (likely decryption) to the user."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-prompts
  '("Enter passphrase for"
    "Enter PIN for"
    "Enter PGP key")
  "Prompts possibly shown to the user for interactive input during decryption."
  :group 'sops-file
  :type '(repeat string))

(defcustom sops-file-auto-mode-regex "\\.enc\\.\\(e?ya?\\|ra\\)ml\\'"
  "Files that we attempt to automatically decrypt. If yaml-mode is available depending on load ordering this might be shadowed by yaml-mode's entry, in which case the hook should suffice."
  :group 'sops-file
  :type 'regexp)

(defcustom sops-file-disable-pinentry nil
  "Have sops prompt for pin directly instead of delegating to pinentry. Counterintuitively, you should set this especially if you use pinentry-tty instead of graphical pinentry, since sops falls back to reading stdin with a prompt instead of delegating to pinentry-tty which immediately fails in a subprocess."
  :group 'sops-file
  :type 'boolean)

(defcustom sops-file-skip-unavailable-smartcards t
  "If prompted to insert a smartcard, skip it instead of prompting the user."
  :group 'sops-file
  :type 'boolean)

;; https://github.com/str4d/age-plugin-yubikey/blob/v0.5.0/i18n/en-US/age_plugin_yubikey.ftl#L182
(defun sops-file--prompt-handler-yubikey-pin ()
  (if-let* ((prev-point (point))
            (curr-point (re-search-forward "Enter pin for.$" nil t))
            (prompt (buffer-substring prev-point curr-point))
            (sops (buffer-process (current-buffer)))
            (passwd (read-passwd prompt))
            (response (format "%s\n" passwd)))
      (process-send-string sops response)
    t))

;; https://github.com/str4d/age-plugin-yubikey/blob/v0.5.0/i18n/en-US/age_plugin_yubikey.ftl#L171
(defun sops-file--prompt-handler-yubikey-insert ()
  (if-let* ((prev-point (point))
            (curr-point (re-search-forward "Please insert.$" nil t))
            (prompt (buffer-substring prev-point curr-point))
            (sops (buffer-process (current-buffer)))
            (response
             (or
              (and sops-file-skip-unavailable-smartcards "2")
              (string (read-char-exclusive prompt (list "1" "2"))))))
      (process-send-string sops response)
    t))

(defcustom sops-file-prompt-handler-functions
  `(sops-file--prompt-handler-yubikey-pin
    sops-file--prompt-handler-yubikey-insert)
  "Sops may repeatedly prompt for additional information during the decryption pass. Users should manipulate this list of functions to handle prompts for their specific scenario. For any successful prompt handling, simply place the point past the full text of the prompt and return t (for run-hook-with-args-until-success to stop)."
  :group 'sops-file
  :type 'hook)

(defvar-local sops-file--is-visiting nil)
(put 'sops-file--is-visiting 'permanent-local t)

(defun sops-file-enable ()
  (unless sops-file--is-visiting
    (format-decode-buffer 'sops-file)))

(defun sops-file-entry-trigger ()
  (if-let* ((_ (file-exists-p buffer-file-name))
            (path buffer-file-name)
            (_
             (with-temp-buffer
               (save-excursion
                 (call-process sops-file-executable nil (current-buffer) nil "filestatus" path))
               ;; if not managed we get :json-false instead of nil, which is truthy
               (eq t (alist-get 'encrypted (json-read-object))))))
      ;; file is managed by sops, attempt to decrypt it
      (sops-file-enable)))

(define-minor-mode sops-file-auto-mode
  "Global minor mode for installing hooks. If yaml-mode is available, add a hook to decrypt on entry of any yaml file if sops can decrypt it. Additionally register an auto-mode-alist entry"
  :global t
  :group 'sops-file
  (cond ((null sops-file-auto-mode)
         (if (fboundp 'yaml-mode)
           (remove-hook 'yaml-mode-hook
                        #'sops-file-entry-trigger)
           (setq auto-mode-alist
                 (cl-delete-if
                  (lambda (entry)
                    (equal (cdr entry) #'sops-file-enable))
                  auto-mode-alist))))
        (t
         (if (fboundp 'yaml-mode)
           (add-hook 'yaml-mode-hook
                     #'sops-file-entry-trigger)
           (add-to-list 'auto-mode-alist `(,sops-file-auto-mode-regex . sops-file-enable))))))

;; we don't remove these on sops-file-auto-mode disable
;; since the user explicitly selects the format
(cl-pushnew
 `(sops-file ,(purecopy "Transparently manipulate sops files")
             nil
             sops-file-decode
             sops-file-encode
             t
             nil)
 format-alist)

(defun sops-file-decode (from to)
  (unless (and (equal from (point-min)) (equal to (point-max)))
    (error "Cannot handle partial decoding of buffer"))
  (unless sops-file--is-visiting
    (setq sops-file--is-visiting t)
    (let* ((stdout (generate-new-buffer " *sops-file-stdout*" t))
           (stderr (generate-new-buffer " *sops-file-stderr*" t))
           (sops
            (make-process
             :name "sops"
             :command `(,@(and sops-file-disable-pinentry
                               ;; we deliberately set to the empty string to trigger a parse
                               ;; error in the gopgagent library sops uses
                               '("env" "GPG_AGENT_INFO=''"))
                        "sops"
                        ,@sops-file-decrypt-args
                        "--output"
                        "/dev/stderr")
             :filter (lambda (_ _)
                       (run-hook-with-args-until-success sops-file-prompt-handler-functions))
             :buffer stdout
             :sentinel #'ignore
             :stderr stderr)))
      (unwind-protect
          (progn
            (set-process-sentinel (get-buffer-process stderr) #'ignore)
            
            (process-send-region sops from to)
            ;; for empty .sops.yaml files sops hangs if we don't send two EOFs
            (cl-loop repeat 2
                     do (process-send-eof sops))
            (with-current-buffer stdout
              (cl-loop ;; repeat 3 
                       while (process-live-p sops)
                       do (accept-process-output sops 1)))
            (if (equal (process-exit-status sops) 0)
                (progn
                  (erase-buffer)
                  (insert-buffer-substring stderr))
              (save-excursion
                (funcall sops-file-error-renderer stderr))))
        (progn
          (kill-buffer stdout)
          (kill-buffer stderr)))
      (funcall sops-file-mode-inferrer)))
  (point-max))

(defun sops-file-encode (from to orig-buf)
  ;; manipulating the output buffer directly
  ;; has proven pretty unreliable, this works reliably
  (let* ((output-buffer (current-buffer))
         ;; save-current-buffer mucks with default-directory in tests,
         ;; and presumably callers can use it so peek default-directory
         ;; off the original buffer
         (default-directory (with-current-buffer orig-buf default-directory))
         (transformed
          (with-temp-buffer
            (insert-buffer-substring orig-buf)
            (apply 'call-process-region
                   from
                   to
                   sops-file-executable
                   t
                   t
                   nil
                   `(,@sops-file-encrypt-args
                     "--filename-override"
                     ,(buffer-file-name orig-buf)))
            (buffer-string))))
    (erase-buffer)
    (insert transformed)
    (setq sops-file--is-visiting nil)
    (point-max)))

(provide 'sops-file)
;;; sops-file.el ends here
