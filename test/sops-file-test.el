;;; sops-file-test.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Ahmad Jarara <ajarara@fastmail.com>

;; Author:  Ahmad Jarara <ajarara@fastmail.com>

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

;; Run with `make test`

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'yaml-mode)
(require 'conf-mode)
(require 'sops-file)


(defun sops-file-test--generate-age-keys ()
    (with-temp-buffer
      (call-process "age-keygen" nil (current-buffer))
      (goto-char (point-min))
      (let (public-key private-key)
        (when (re-search-forward "^# public key: \\(.+\\)$" nil t)
          (setq public-key (match-string 1)))
        (when (re-search-forward "^\\(AGE-SECRET-KEY-.*\\)$" nil t)
          (setq private-key (match-string 1)))
        (list public-key private-key))))

(defun sops-file-test--yaml-for-key (public-key)
  (format "
creation_rules:
  - age: %s
" public-key))

(defmacro with-age-encrypted-file (relpath contents &rest body)
  (declare (debug t) (indent defun))
  (let ((keys-sym (gensym))
        (sops-yaml-sym (gensym))
        (test-dir-sym (gensym))
        (identity-file-sym (gensym)))
  `(let* ((,keys-sym (sops-file-test--generate-age-keys))
          (,sops-yaml-sym (sops-file-test--yaml-for-key (car ,keys-sym)))
          (,test-dir-sym (make-temp-file "sops-file-test--" t))
          (default-directory ,test-dir-sym)
          (,identity-file-sym (expand-file-name "identity.txt")))
     (with-temp-file ,identity-file-sym
       (insert (cadr ,keys-sym)))
     (with-temp-file (expand-file-name ".sops.yaml")
       (insert ,sops-yaml-sym))
     (with-temp-file ,relpath
       (insert ,contents))
     (process-lines "sops" "encrypt" "-i" ,relpath)
     (setenv "SOPS_AGE_KEY_FILE" ,identity-file-sym)
     ,@body
     (delete-directory ,test-dir-sym t))))

(defvar sops-file-test-passphrase-key "passphrase")

(defmacro with-file-encrypted-with-passphrase-key (relpath contents &rest body)
  (declare (debug t) (indent defun))
  (let ((keys-sym (gensym))
        (sops-yaml-sym (gensym))
        (test-dir-sym (gensym))
        (identity-file-sym (gensym)))
  `(let* ((,keys-sym (sops-file-test--generate-age-keys))
          (,sops-yaml-sym (sops-file-test--yaml-for-key (car ,keys-sym)))
          (,test-dir-sym (make-temp-file "sops-file-test--" t))
          (default-directory ,test-dir-sym)
          (,identity-file-sym (expand-file-name "identity.txt")))
     (with-temp-buffer
       (let ((age (start-process "age" (current-buffer) "age" "-p" "-o" ,identity-file-sym))
             ;; passphrase, confirmation, contents
             (input (list sops-file-test-passphrase-key
                          sops-file-test-passphrase-key
                          (cadr ,keys-sym))))
         (dolist (in input)
           (process-send-string age (format "%s\n" in)))
         (process-send-eof age)
         ;; simply wait until process-exit
         (while (not (equal (process-status age) 'exit))
           (accept-process-output age 3 nil nil))))
     (with-temp-file (expand-file-name ".sops.yaml")
       (insert ,sops-yaml-sym))
     (with-temp-file ,relpath
       (insert ,contents))
     (process-lines "sops" "encrypt" "-i" ,relpath)
     (setenv "SOPS_AGE_KEY_FILE" (expand-file-name "identity.txt"))
     ;; disable gpg-agent pinentry
     (setenv "GPG_AGENT_INFO" "")
     ,@body
     ;; preserve directory on body failure, to aid debugging
     (delete-directory ,test-dir-sym t))))

(defmacro with-yaml-mode-disabled (&rest body)
  (declare (debug t) (indent defun))
  (let ((yaml-mode-sym (gensym)))
    `(let ((,yaml-mode-sym (symbol-function 'yaml-mode))
            (auto-mode-alist
             (cl-remove-if
              (lambda (entry)
                (equal (cdr entry) 'yaml-mode))
              auto-mode-alist)))
        (unwind-protect
            (progn
              (fmakunbound 'yaml-mode)
              ,@body)
          (fset 'yaml-mode ,yaml-mode-sym)))))

(ert-deftest sops-file-test--read-file ()
  (with-age-encrypted-file "my-file.enc.yaml" "key: value\n"
    (format-find-file "my-file.enc.yaml" 'sops-file)
    (should (equal (buffer-string) "key: value\n"))
    (should (equal major-mode 'yaml-mode))))

(ert-deftest sops-file-test--major-mode-respects-contents ()
  (with-age-encrypted-file "opaque-name" "#!/usr/bin/env sh"
    (format-find-file "opaque-name" 'sops-file)
    (should (equal (buffer-string) "#!/usr/bin/env sh"))
    (should (equal major-mode 'sh-mode))))

(ert-deftest sops-file-test--updates-are-saved ()
  (with-age-encrypted-file "opaque-name" "#!/usr/bin/env sh"
    (save-current-buffer
      (format-find-file "opaque-name" 'sops-file)
      (replace-string "sh" "awk")
      (save-buffer)
      (kill-buffer))
    (format-find-file "opaque-name" 'sops-file)
    (should (equal major-mode 'awk-mode))))

(ert-deftest sops-file-test--yaml-mode-disabled ()
  (with-yaml-mode-disabled
    (sops-file-auto-mode)
    (with-age-encrypted-file "my-file.enc.yaml" "key: value\n"
      (find-file "my-file.enc.yaml")
      (should (equal (buffer-string) "key: value\n"))
      (should (equal major-mode 'fundamental-mode)))))

(ert-deftest sops-file-test--passphrase-read-file ()
  (with-file-encrypted-with-passphrase-key "my-file.enc.yaml" "key: value\n"
    (ert-simulate-keys (format "%s\n" sops-file-test-passphrase-key)
      (format-find-file "my-file.enc.yaml" 'sops-file))
    (should (equal (buffer-string) "key: value\n"))
    (should (equal major-mode 'yaml-mode))))

(provide 'sops-file-test)
