;;; sops-file.el --- Transparently manipulate sops encrypted files -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ahmad Jarara <ajarara@fastmail.com>

;; Author:  Ahmad Jarara <ajarara@fastmail.com>
;; Keywords: convenience, programming
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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
(require 'json)

(defgroup sops-file nil
  "Transparently manipulate SOPS files."
  :prefix 'sops-file
  :group 'convenience)

(defcustom sops-file-executable
  (cl-constantly "sops")
  "The sops executable we use to encrypt and decrypt.
Called in the buffer we are formatting."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-decrypt-args
  (cl-constantly '("decrypt"))
  "Decrypt arguments for sops.
Called in the buffer we are formatting, where the text is still encrypted."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-encrypt-args
  (cl-constantly '("encrypt"))
  "Encrypt arguments for sops.
Called in the buffer we are formatting, where the text is decrypted."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-name-inferrer
  (lambda ()
    (or buffer-file-name
        (buffer-name)))
  "Infer file name based off the current buffer."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-mode-inferrer
  (lambda ()
    ;; we post this because we want it to run _after_ the format has been applied, so that we don't attempt to sops decrypt on re-entry of the hook
    (run-at-time 0 nil
                 (lambda ()
                   (let ((buffer-file-name
                          (string-replace ".enc" "" (funcall sops-file-name-inferrer))))
                     (normal-mode)))))
  "Manipulate the mode of the file after decoding it."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-decryption-error-renderer
  (lambda (stderr-buf)
    (let* ((stdout-buf (current-buffer))
           (sops-process (get-buffer-process stdout-buf))
           (process-hanging (process-live-p sops-process)))
      (with-current-buffer (get-buffer-create "*sops-file-error*")
        (let ((buffer-read-only nil))
          (erase-buffer)
          (when process-hanging
            (insert "sops-file.el: Process is hanging with possibly unhandled prompts. Write a prompt handler to handle these, and consider upstreaming for your use case.\nsops-file.el: stdout output follows\n")
            (insert-buffer-substring stdout-buf)
            (insert "\nsops-file.el: stderr output follows\n"))
          (insert-buffer-substring stderr-buf))
        (special-mode))
      (error "Could not decrypt visited file, see *sops-file-error* for sops output")))
  "Report sops error (likely decryption) to the user."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-auto-mode-regex "\\.enc\\.\\(e?ya?\\|ra\\)ml\\'"
  "Files that we attempt to automatically decrypt.
If `yaml-mode' is available, depending on load ordering, this might be
shadowed by yaml-mode's entry, in which case the hook should suffice."
  :group 'sops-file
  :type 'regexp)

(defcustom sops-file-disable-pinentry nil
  "Have sops prompt for pin directly instead of delegating to pinentry."
  :group 'sops-file
  :type 'boolean)

(defcustom sops-file-skip-unavailable-smartcards t
  "If prompted to insert a smartcard, skip it instead of prompting the user."
  :group 'sops-file
  :type 'boolean)

;; https://github.com/str4d/age-plugin-yubikey/blob/v0.5.0/i18n/en-US/age_plugin_yubikey.ftl#L182
(defun sops-file--prompt-handler-yubikey-pin ()
  "Handles yubikey pin prompt."
  (if-let* ((_ (re-search-forward "Enter pin for.*" nil t))
            (prompt (match-string 0))
            (sops (get-buffer-process (current-buffer)))
            (passwd (read-passwd prompt))
            (response (format "%s\n" passwd)))
      (not (process-send-string sops response))))

;; https://github.com/str4d/age-plugin-yubikey/blob/v0.5.0/i18n/en-US/age_plugin_yubikey.ftl#L171
(defun sops-file--prompt-handler-yubikey-insert ()
  "Handles yubikey pin insertion prompt."
  (if-let* ((_ (re-search-forward "Please insert.*" nil t))
            (prompt (match-string 0))
            (sops (get-buffer-process (current-buffer)))
            (response
             (or
              (and sops-file-skip-unavailable-smartcards "2")
              (string (read-char-choice prompt (list ?1 ?2))))))
      (not (process-send-string sops response))))

(defun sops-file--prompt-handler-passphrase-identity ()
  "Handles encrypted age identity passphrase prompt."
  (if-let* ((_ (re-search-forward "Enter passphrase for identity.*" nil t))
            (prompt (match-string 0))
            (sops (get-buffer-process (current-buffer)))
            (passwd (read-passwd prompt))
            (response (format "%s\n" passwd)))
      (not (process-send-string sops response))))

(defcustom sops-file-prompt-handler-functions
  `(sops-file--prompt-handler-yubikey-pin
    sops-file--prompt-handler-yubikey-insert
    sops-file--prompt-handler-passphrase-identity)
  "Sops may repeatedly prompt for additional information during a decryption.
Users should manipulate this list of functions to handle prompts for
their specific scenario.  For any successful prompt handling, simply
place the point past the full text of the prompt and return t (for
`run-hook-with-args-until-success' to stop)."
  :group 'sops-file
  :type 'hook)


(defcustom sops-file-decryption-prompt-handler
  (lambda (beg _ _)
    ;; we do this to counteract the default behavior of the standard filter moving up point to process-mark
    (when (= (point) (point-max))
      (goto-char beg))
    (run-hook-with-args-until-success 'sops-file-prompt-handler-functions))
  "Installed as a buffer local hook for a decryption pass to handle any prompts."
  :group 'sops-file
  :type 'function)

(defcustom sops-file-decryption-process-manager
  (lambda ()
    (let ((sops (get-buffer-process (current-buffer))))
      (cl-loop repeat 10
               while (process-live-p sops)
               do (accept-process-output sops 1))))
  "Holds up the decode call for any interactive portions to take place.
For example pins, passphrases, yubikey touches, network calls, pinentry.
Called in sops' buffer."
  :group 'sops-file
  :type 'function)

(defun sops-file-enable ()
  "Used as an entry point for `sops-file-auto-mode' and `sops-file-entry-trigger'."
  (unless (memq 'sops-file buffer-file-format)
    (format-decode-buffer 'sops-file)))

(defun sops-file-entry-trigger ()
  "A function that can be generally placed on any hook.
This determines whether to apply the sops-file format."
  ;; a fresh file with the right name should trigger sops-file format application
  ;; but more generally, we can query sops filestatus to determine eligibility of format application
  (let ((sops-file-name (funcall sops-file-name-inferrer)))
    (when (or (string-match sops-file-auto-mode-regex sops-file-name)
              (and
               (file-exists-p sops-file-name)
               (with-temp-buffer
                 (save-excursion
                   (call-process (funcall sops-file-executable nil (current-buffer) nil "filestatus" sops-file-name)))
                 ;; if not managed we get :json-false instead of nil, which is truthy
                 (eq t (alist-get 'encrypted (json-read-object))))))
      (sops-file-enable))))

;;;###autoload
(define-minor-mode sops-file-auto-mode
  "Global minor mode for installing hooks.
If `yaml-mode' is available, add a hook to decrypt on entry of any yaml
file if sops can decrypt it.  Additionally register an `auto-mode-alist' entry"
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
  "The decode portion of the sops file format.
We require FROM to be point-min and TO to be point-max, as sops does not
expect sops ciphertext to be embedded in larger cleartext (excepting partial
decryption, which is handled by sending everything over anyway."
  (unless (and (equal from (point-min)) (equal to (point-max)))
    (error "Cannot handle partial decoding of buffer"))
  (if (or (= from to)
          (memq 'sops-file buffer-file-format))
      ;; nothing to decode
      (point-max)
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
                        ,@(funcall sops-file-decrypt-args)
                        "--filename-override"
                        ,(funcall sops-file-name-inferrer)
                        "--output"
                        "/dev/stderr")
             :buffer stdout
             :sentinel #'ignore
             :stderr stderr)))
      (unwind-protect
          (progn
            (set-process-sentinel (get-buffer-process stderr) #'ignore)
            ;; change functions have been more reliable than a custom process filter
            (with-current-buffer stdout
              (add-hook 'after-change-functions sops-file-decryption-prompt-handler nil t))
            (process-send-region sops from to)
            ;; for empty .sops.yaml files, sops hangs if we don't send two EOFs
            (cl-loop repeat 2
                     do (process-send-eof sops))
            (with-current-buffer stdout
              (funcall sops-file-decryption-process-manager))
            (if (and
                 (equal (process-exit-status sops) 0)
                 (not (process-live-p sops)))
                (progn
                  (erase-buffer)
                  (insert-buffer-substring stderr)
                  (funcall sops-file-mode-inferrer))
              (save-excursion
                (with-current-buffer stdout
                  (funcall sops-file-decryption-error-renderer stderr)))))
        (progn
          ;; in normal scenarios it's already dead, in unhandled prompt scenarios it won't be
          (ignore-errors
            (kill-process sops))
          (kill-buffer stdout)
          (kill-buffer stderr)))
      (point-max))))

(defun sops-file-encode (from to orig-buf)
  "The encode version of the sops-file format.
At the end of the encryption pass, we completely replace the contents
of ORIG-BUF with the results.  We require FROM to be point-min and TO
to be point-max, as the decode portion does with the same reasoning"
  (unless (and (equal from (point-min)) (equal to (point-max)))
    (error "Cannot handle partial decoding of buffer"))
  ;; manipulating the output buffer directly
  ;; has proven pretty unreliable, this works reliably
  (let* ((default-directory (with-current-buffer orig-buf default-directory))
         (transformed
          (with-temp-buffer
            (insert-buffer-substring orig-buf)
            (apply 'call-process-region
                   from
                   to
                   (funcall sops-file-executable)
                   t
                   t
                   nil
                   `(,@(funcall sops-file-encrypt-args)
                     "--filename-override"
                     ,(with-current-buffer orig-buf (funcall sops-file-name-inferrer))))
            (buffer-string))))
    (erase-buffer)
    (insert transformed)
    ;; unsure why the format infrastructure does not remove the format
    (setq buffer-file-format (cl-remove 'sops-file buffer-file-format))
    (point-max)))

(provide 'sops-file)
;;; sops-file.el ends here
