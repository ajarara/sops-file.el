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

;; Run with `make test`, these tests won't run reliably if at all in
;; a host emacs

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

(defmacro with-yaml-mode-unavailable (&rest body)
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

(defmacro with-sops-file-auto-mode (&rest body)
  (declare (debug t) (indent defun))
  `(unwind-protect
       (progn
         (sops-file-auto-mode 1)
         ,@body)
     (sops-file-auto-mode -1)))

(ert-deftest sops-file-test--read-file ()
  (let ((relpath "read-file.enc.yaml")
        (contents "key: value\n"))
    (with-age-encrypted-file relpath contents
      (format-find-file relpath 'sops-file)
      (should (equal (buffer-string) contents))
      (should (equal major-mode 'yaml-mode)))))

(ert-deftest sops-file-test--major-mode-respects-contents ()
  (let ((relpath "respects-contents"))
    (with-age-encrypted-file relpath "#!/usr/bin/env sh"
      (format-find-file relpath 'sops-file)
      (should (equal (buffer-string) "#!/usr/bin/env sh"))
      (should (equal major-mode 'sh-mode)))))

(ert-deftest sops-file-test--updates-are-saved ()
  (let ((relpath "updates-saved"))
    (with-age-encrypted-file relpath "#!/usr/bin/env sh"
      (save-current-buffer
        (format-find-file relpath 'sops-file)
        (replace-string "sh" "awk")
        (save-buffer)
        (kill-buffer))
      (format-find-file relpath 'sops-file)
      (should (equal major-mode 'awk-mode)))))

(ert-deftest sops-file-test--auto-mode-entry-point ()
  (let ((relpath "auto-mode-entry.enc.yaml"))
    (with-yaml-mode-unavailable
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (find-file relpath)
          (should (equal (buffer-string) "key: value\n"))
          (should (equal major-mode 'fundamental-mode)))))))

(ert-deftest sops-file-test--yaml-mode-entry-point ()
  (let ((relpath "yaml-mode-entry.enc.yaml"))
    (with-sops-file-auto-mode
      (with-age-encrypted-file relpath "key: value\n"
        (find-file relpath)
        (should (equal (buffer-string) "key: value\n"))
        (should (equal major-mode 'yaml-mode))))))

(ert-deftest sops-file-test--passphrase-read-file ()
  (let ((relpath "passphrase-read-file.enc.yaml"))
    (with-file-encrypted-with-passphrase-key relpath "key: value\n"
      (ert-simulate-keys (format "%s\n" sops-file-test-passphrase-key)
        (format-find-file relpath 'sops-file))
      (should (equal (buffer-string) "key: value\n"))
      (should (equal major-mode 'yaml-mode)))))

;; (ert-deftest sops-file-test--file-does-not-exist-is-silent ())

;; (ert-deftest sops-file-test--yaml-is-not-managed-by-sops ())

;; (ert-deftest sops-file-test--cannot-decrypt-shows-error ())

;; fails with "Unknown format sops-file" in format.el
;; reproduced by toggling sops-file-auto-mode
;; might be stale references or something?

;; (ert-deftest sops-file-test--re-entering-does-not-redecode ()
;;   (let ((relpath "re-entering-no-decode.enc.yaml"))
;;     (with-sops-file-auto-mode
;;       (with-age-encrypted-file relpath "key: value\n"
;;         (find-file relpath)
;;         (format-find-file relpath 'sops-file)
;;         (should (equal (buffer-string "key: value\n")))))))

(provide 'sops-file-test)
