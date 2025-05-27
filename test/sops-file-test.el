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
(require 'json)


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

(defun sops-file-test--write-sops-yaml-for-keys (public-keys)
  (with-temp-file ".sops.yaml"
    (insert "
creation_rules:
  - age:")
    (dolist (key public-keys)
      (insert (format " %s," key)))
    (delete-char -1)))

(defmacro with-sops-identity (keys &rest body)
  (declare (debug t) (indent defun))
  (let ((old-key-file-sym (gensym)))
    `(let ((,old-key-file-sym (getenv "SOPS_AGE_KEY_FILE")))
       (with-temp-file "identity.txt"
         (insert
          (string-join
           (mapcar #'cadr ,keys)
           "\n")))
       (unwind-protect
           (progn
             (setenv "SOPS_AGE_KEY_FILE" "identity.txt")
             ,@body)
         (setenv "SOPS_AGE_KEY_FILE" ,old-key-file-sym)))))

;; should be the outermost macro call -- we set default directory here 
(defmacro with-sops-file-directory (name &rest body)
  (declare (debug t) (indent defun))
  (let ((sops-file-test-root (expand-file-name  "sops-file-tests" temporary-file-directory)))
    `(progn
       (mkdir ,sops-file-test-root t)
       (let* ((default-directory
               (make-temp-file
                (expand-file-name ,name ,sops-file-test-root) t)))
         (unwind-protect
             (progn
               ,@body)
           (ignore-errors
             (kill-buffer "*sops-file-error*")))
         ;; preserve directory on body failure, to aid debugging
         (delete-directory default-directory t)))))

(defmacro with-age-encrypted-file (relpath contents &rest body)
  (declare (debug t) (indent defun))
  (let ((keys-sym (gensym)))
    `(let* ((,keys-sym (sops-file-test--generate-age-keys)))
       (sops-file-test--write-sops-yaml-for-keys (list (car ,keys-sym)))
       (with-temp-file ,relpath
         (insert ,contents))
       (process-lines "sops" "encrypt" "-i" ,relpath)
       (with-sops-identity (list ,keys-sym)
         ,@body))))

(defmacro with-disabled-gpg-agent (&rest body)
  (declare (debug t) (indent defun))
  (let ((disabled-gpg-agent-info-sym (gensym)))
    `(let ((,disabled-gpg-agent-info-sym (getenv "GPG_AGENT_INFO")))
       (unwind-protect
           (progn
             ;; we deliberately set to the empty string to trigger a parse
             ;; error in the gopgagent library sops uses
             (setenv "GPG_AGENT_INFO" "")
             ,@body)
         (setenv "GPG_AGENT_INFO" ,disabled-gpg-agent-info-sym)))))

(defvar sops-file-test-passphrase-key "passphrase")

(defun encrypt-identity-file ()
  (let ((age (start-process "age" nil "age" "-p" "-o" "identity.txt.age" "identity.txt")))
    (dotimes (i 2)
      (process-send-string age (format "%s\n" sops-file-test-passphrase-key)))
    ;; block until encryption completes
    (while (not (equal (process-status age) 'exit))
      (accept-process-output age 3 nil nil))
    ;; clobber identity.txt
    (rename-file "identity.txt.age" "identity.txt" t)))

(defmacro with-encrypted-identity (&rest body)
  "Encrypt the identity file with sops-file-test-passphrase-key: decryptions will require a passphrase to be input. See `with-passphrase-input'."
  (declare (debug t) (indent defun))
  `(progn
     (encrypt-identity-file)
     (with-disabled-gpg-agent
       ,@body)))

(defmacro with-passphrase-input (passphrase &rest body)
  (declare (debug t) (indent defun))
  `(ert-simulate-keys (format "%s\n" ,passphrase)
     ,@body))

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
    (with-sops-file-directory "read-file"
      (with-age-encrypted-file relpath contents
        (format-find-file relpath 'sops-file)
        (should (equal (buffer-string) contents))
        (should (equal major-mode 'yaml-mode))))))

(ert-deftest sops-file-test--major-mode-respects-contents ()
  (let ((relpath "respects-contents"))
    (with-sops-file-directory "respects-contents"
      (with-age-encrypted-file relpath "#!/usr/bin/env sh"
        (format-find-file relpath 'sops-file)
        (should (equal (buffer-string) "#!/usr/bin/env sh"))
        (should (equal major-mode 'sh-mode))))))

(ert-deftest sops-file-test--updates-are-saved ()
  (let ((relpath "updates-saved"))
    (with-sops-file-directory "updates-saved"
      (with-age-encrypted-file relpath "#!/usr/bin/env sh"
        (save-current-buffer
          (format-find-file relpath 'sops-file)
          (while (search-forward "sh" nil t)
            (replace-match "awk" nil t))
          (save-buffer)
          (kill-buffer))
        (format-find-file relpath 'sops-file)
        (should (equal major-mode 'awk-mode))))))

(ert-deftest sops-file-test--auto-mode-entry-point ()
  (let ((relpath "auto-mode-entry.enc.yaml"))
    (with-sops-file-directory "auto-mode-entry-point"
      (with-yaml-mode-unavailable
        (with-sops-file-auto-mode
          (with-age-encrypted-file relpath "key: value\n"
            (find-file relpath)
            (should (equal (buffer-string) "key: value\n"))
            (should (equal major-mode 'fundamental-mode))))))))

(ert-deftest sops-file-test--yaml-mode-entry-point ()
  (let ((relpath "yaml-mode-entry.enc.yaml"))
    (with-sops-file-directory "yaml-mode-entry"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (find-file relpath)
          (should (equal (buffer-string) "key: value\n"))
          (should (equal major-mode 'yaml-mode)))))))


(ert-deftest sops-file-test--passphrase-read-file ()
  (let ((relpath "passphrase-read-file.enc.yaml"))
    (with-sops-file-directory "passphrase-read-file"
      (with-age-encrypted-file relpath "key: value\n"
        (with-encrypted-identity
          (with-passphrase-input sops-file-test-passphrase-key
            (format-find-file relpath 'sops-file))
          (should (equal (buffer-string) "key: value\n"))
          (should (equal major-mode 'yaml-mode)))))))

(ert-deftest sops-file-test--file-does-not-exist-is-silent ()
  (with-sops-file-auto-mode
    (find-file "ex.enc.yaml")
    (should (equal (buffer-string) ""))))

(ert-deftest sops-file-test--yaml-is-not-managed-by-sops ()
  (with-sops-file-directory "yaml-is-not-managed"
    (with-sops-file-auto-mode
      (with-age-encrypted-file "_" "_"
        (find-file ".sops.yaml")
        (let ((retrieved (buffer-string))
              (on-disk (with-temp-buffer
                         (insert-file-contents ".sops.yaml")
                         (buffer-string))))
          (should (equal retrieved on-disk)))))))

(ert-deftest sops-file-test--cannot-decrypt-shows-error-in-sops-file-errors ()
  (let ((relpath "cannot-decrypt.enc.yaml"))
    (with-sops-file-directory "cannot-decrypt"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (setenv "SOPS_AGE_KEY_FILE")
          (find-file relpath)
          (with-current-buffer "*sops-file-error*"
            (let ((expected-failure "Failed to get the data key required to decrypt the SOPS file."))
              (should (equal (buffer-substring (point-min) (1+ (length expected-failure))) expected-failure)))))))))

(ert-deftest sops-file-test--bad-passwd-shows-error-in-sops-file-errors ()
  (let ((relpath "bad-passwd.enc.yaml"))
    (with-sops-file-directory "passphrase-read-file"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (with-encrypted-identity
            (with-passphrase-input "not-the-phrase"
              (find-file relpath))
            (with-current-buffer "*sops-file-error*"
              (goto-char (point-min))
              (should (re-search-forward "incorrect passphrase")))))))))

(ert-deftest sops-file-test--major-mode-in-filename-is-respected-after-decryption ()
  (let ((relpath "my-secret-package.enc.el"))
    (with-sops-file-directory "major-mode-in-filename"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath ""
          (find-file relpath)
          (should (equal major-mode 'emacs-lisp-mode)))))))

(ert-deftest sops-file-test--re-entering-does-not-redecode ()
  (let ((relpath "re-entering-no-decode.enc.yaml"))
    (with-sops-file-directory "re-entering-does-not-redecode"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (find-file relpath)
          (format-find-file relpath 'sops-file)
          (should (equal (buffer-string) "key: value\n")))))))

(ert-deftest sops-file-test--re-encode-allows-decode ()
  (let ((relpath "re-encode-allows-decode.enc.yaml"))
    (with-sops-file-directory "re-encode-allows-decode"
      (with-sops-file-auto-mode
        (with-age-encrypted-file relpath "key: value\n"
          (find-file-literally relpath)
          (format-decode-buffer 'sops-file)
          (should (equal (buffer-string) "key: value\n"))
          (format-encode-buffer 'sops-file)
          (should (not (equal (buffer-string) "key: value\n")))
          (format-decode-buffer 'sops-file)
          (should (equal (buffer-string) "key: value\n")))))))

;; (ert-deftest sops-file-test--file-creation ()
;;   (let ((relpath "non-existent-file.enc.yaml"))
;;     (with-sops-file-directory "file-creation"
;;       (sops-file-test--write-sops-yaml-for-keys)
;;       (format-find-file relpath 'sops-file))))

(provide 'sops-file-test)
