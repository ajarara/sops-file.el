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

(defgroup sops-file nil "Transparently manipulate SOPS files" :prefix 'sops-file :group 'convenience)

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

(defvar sops-file-auto-mode-regex
  "\\.enc\\.\\(e?ya?\\|ra\\)ml\\'"
  "Files that we attempt to automatically decrypt. If yaml-mode is available depending on load ordering this might be shadowed by yaml-mode's entry, in which case the hook should suffice.")

(defvar-local sops-file--is-visiting nil)
(put 'sops-file--is-visiting 'permanent-local t)

(defun sops-file-enable ()
  (unless sops-file--is-visiting
    (format-decode-buffer 'sops-file)))

(defun sops-file--yaml-entry-hook ()
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
         (when (fboundp 'yaml-mode)
           (remove-hook 'yaml-mode-hook
                        #'sops-file--yaml-entry-hook))
         (cl-delete-if
          (lambda (entry)
            (equal (cdr entry) #'sops-file-enable))
          auto-mode-alist))
        (t
         (when (fboundp 'yaml-mode)
           (add-hook 'yaml-mode-hook
                     #'sops-file--yaml-entry-hook))
         (add-to-list 'auto-mode-alist `(,sops-file-auto-mode-regex . sops-file-enable)))))

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
    (error "Cannot handle partial decoding"))
  (unless sops-file--is-visiting
    (setq sops-file--is-visiting t)
    (let* ((stdout (generate-new-buffer " *sops-file-stdout*" t))
           (stderr (generate-new-buffer " *sops-file-stderr*" t))
           (sops
            (make-process
             :name "sops"
             :command `("sops"
                        ,@sops-file-decrypt-args
                        "--filename-override"
                        ,buffer-file-name
                        "--output"
                        "/dev/stderr")
             :buffer stdout
             :sentinel #'ignore
             :stderr stderr)))
      (set-process-sentinel (get-buffer-process stderr) #'ignore)
      (process-send-region sops from to)
      (process-send-eof sops)
      (accept-process-output sops 1)
      (with-current-buffer stdout
        (if-let ((_
                  (cl-loop
                   for prompt in '("Enter passphrase for"
                                   "Enter PIN for")
                   when (save-excursion
                          (goto-char (point-min))
                          (re-search-forward prompt nil t))
                   return t))
                 (passwd (read-passwd (buffer-string))))
            (process-send-string
                 sops
                 (format "%s\n" passwd))
          ))
      (while (not (equal (process-status sops) 'exit))
        (accept-process-output sops 1))
      (erase-buffer)
      (insert-buffer stderr)))
  (funcall sops-file-mode-inferrer)
  (point-max))

(defun sops-file-encode (from to orig-buf)
  ;; manipulating the output buffer directly
  ;; has proven pretty unreliable, this works reliably
  (let* ((output-buffer (current-buffer))
         (transformed
          (with-temp-buffer
            (insert-buffer orig-buf)
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
    (point-max)))

(provide 'sops-file)
;;; sops-file.el ends here
