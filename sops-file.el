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
    (let* ((buffer-file-name
            (string-replace ".enc" "" buffer-file-name)))
      (normal-mode t)))
  "Manipulate the mode of the file after decoding it"
  :group 'sops-file
  :type 'function)

(define-minor-mode sops-file-auto-mode
  "Global minor mode for installing hooks"
  :global t
  :group 'sops-file
  (cond ((null sops-file-mode)
         ;; remove yaml mode hook
         ;; remove auto-mode-alist entry for .enc.yml
         )
        (t
         ;; add yaml mode hook
         ;; add auto-mode-alist entry for .enc.yml
         )))

(cl-pushnew
 `(sops-file ,(purecopy "Transparently manipulate sops files")
             nil
             sops-file-decode
             sops-file-encode
             t
             nil)
 format-alist)

(defun sops-file-is-applicable-p (path)
  (when path
    (with-temp-buffer
      (save-excursion
        (call-process sops-file-executable nil (current-buffer) nil "filestatus" path))
      (alist-get 'encrypted (json-read-object)))))

(defun sops-file-decode (from to)
  (unless (and (equal from (point-min)) (equal to (point-max)))
    (error "Cannot handle partial decoding"))
  (apply 'call-process-region
         from
         to
         sops-file-executable
         t
         (current-buffer)
         nil
         `(,@sops-file-decrypt-args
           "--filename-override"
           ,buffer-file-name))
  (funcall sops-file-mode-inferrer)
  (point-max))


(defun sops-file-encode (from to orig-buf)
  ;; manipulating the output buffer directly
  ;; has proven pretty unreliable, this works
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
    (when (buffer-live-p (current-buffer))
      (erase-buffer)
      (insert transformed))
    (point-max)))

  ;; (define-derived-mode sops-file-mode fundamental-mode)
  ;; 
  ;; write-file-functions

  ;; (put 'insert-file-contents 'epa-file 'epa-file-insert-file-contents)

  ;; (put 'write-region 'sops-file 'sops-file-write-region)


  (provide 'sops-file)
;;; sops-file.el ends here
