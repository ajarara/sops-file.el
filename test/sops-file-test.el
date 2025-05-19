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
(require 'yaml-mode)
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

;; there are three tested setups (and one untested, yubikeys, since they can't be automated):
;; a file encrypted with an age key
;; a file encrypted with a passphrase
;; a file encrypted with an age key that is itself encrypted with a passphrase

;; (defmacro with-age-encrypted-file)
;; (defmacro with-age-passphrase-encrypted-file)
;; (defmacro with-age-file-encrypted-with-passphrase-key)

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
          (,identity-file-sym
           (with-temp-file (expand-file-name "identity.txt")
             (insert (cadr ,keys-sym))))
          (_
           (with-temp-file (expand-file-name ".sops.yaml")
             (insert ,sops-yaml-sym)))
          (_
           (with-temp-file ,relpath
             (insert ,contents)))
          (_
           (process-lines
            "sops" "encrypt" "-i" ,relpath))
          (_ (setenv "SOPS_AGE_KEY_FILE" (expand-file-name "identity.txt"))))
     ,@body
     (delete-directory ,test-dir-sym t))))
     

(setq sops-file-test--example-yaml "my-key: my-value\n")

(ert-deftest sops-file-test--read-file ()
  (with-age-encrypted-file "my-file.enc.yaml" sops-file-test--example-yaml
    
    (format-find-file "my-file.enc.yaml" 'sops-file)
    
    (should (equal (buffer-string) sops-file-test--example-yaml))
    (should (equal 'yaml-mode major-mode))))

(provide 'sops-file-test)
