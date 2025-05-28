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

(defun sops-file-test--generate-age-keys (&optional count)
  (cl-loop
   repeat (or count 1)
   collect
   (with-temp-buffer
     (call-process "age-keygen" nil (current-buffer))
     (goto-char (point-min))
     (let (public-key private-key)
       (when (re-search-forward "^# public key: \\(.+\\)$" nil t)
         (setq public-key (match-string 1)))
       (when (re-search-forward "^\\(AGE-SECRET-KEY-.*\\)$" nil t)
         (setq private-key (match-string 1)))
       (list public-key private-key)))))

(defun sops-file-test--write-sops-yaml-for-keys (keys)
  (with-temp-file ".sops.yaml"
    (insert "
creation_rules:
  - age:")
    (dolist (key keys)
      (insert (format " %s," (car key))))
    (delete-char -1)))

(defun sops-file-test--write-identity-for-keys (keys)
  (with-temp-file "identity.txt"
    (insert
     (string-join
      (mapcar #'cadr keys)
      "\n"))))

;; in the future we should extend this to support multiple kinds of keys
;; but for now, we'll stick with age: gpg needs strong isolation from the daemon
;; ssh keys might be easier to test.
(defun sops-file-test-setup-single-age-key ()
  (let ((keys (sops-file-test--generate-age-keys)))
    (sops-file-test--write-sops-yaml-for-keys keys)
    (sops-file-test--write-identity-for-keys keys)))

(defmacro with-sops-identity (keys &rest body)
  (declare (debug t) (indent defun))
  (let ((old-key-file-sym (gensym)))
    `(let ((,old-key-file-sym (getenv "SOPS_AGE_KEY_FILE")))
       (sops-file-test--write-identity-for-keys ,keys)
       (unwind-protect
           (progn
             (setenv "SOPS_AGE_KEY_FILE" "identity.txt")
             ,@body)
         (setenv "SOPS_AGE_KEY_FILE" ,old-key-file-sym)))))

(defmacro with-sops-identity-file (&rest body)
  (declare (debug t) (indent defun))
  (let ((old-key-file-sym (gensym)))
    `(let ((,old-key-file-sym (getenv "SOPS_AGE_KEY_FILE")))
       (unwind-protect
           (progn
             (setenv "SOPS_AGE_KEY_FILE" "identity.txt")
             ,@body)
         (setenv "SOPS_AGE_KEY_FILE" ,old-key-file-sym)))))

(defmacro with-output-to-encrypted-sops-file (file &rest body)
  (declare (debug t) (indent defun))
  `(with-temp-buffer
     ,@body
     (should
      (eq 0
          (call-process-region (point-min)
                               (point-max)
                               "sops"
                               nil
                               `(:file ,,file)
                               nil
                               "encrypt"
                               "--filename-override"
                               ,file)))))

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

(defun sops-file-test-age-encrypt-identity-file ()
  (let ((age (start-process "age" nil "age" "-p" "-o" "identity.txt.age" "identity.txt")))
    ;; we send twice because age requests confirmation
    (dotimes (i 2)
      (process-send-string age (format "%s\n" sops-file-test-passphrase-key)))
    ;; block until encryption completes
    (while (not (equal (process-status age) 'exit))
      (accept-process-output age 0.5 nil nil))
    ;; clobber identity.txt
    (rename-file "identity.txt.age" "identity.txt" t)))

(defmacro with-encrypted-identity (&rest body)
  "Encrypt the identity file with sops-file-test-passphrase-key: decryptions will require a passphrase to be input. See `with-passphrase-input'."
  (declare (debug t) (indent defun))
  `(progn
     (sops-file-test-age-encrypt-identity-file)
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

;; the default test setup -- creates a directory under sops-test-file
;; and sets the default directory to it.
;; we don't bother updating docstrings.
(cl-defmacro sops-file-test (name () &body body)
  (declare (debug t) (indent defun))
  (let ((sops-file-test-root (expand-file-name "sops-file-tests" temporary-file-directory)))
    `(ert-deftest ,(intern (format "sops-file-test--%s" name)) ()
       (progn
         (mkdir ,sops-file-test-root t)
         (let ((default-directory
                (make-temp-file
                 (expand-file-name ,(symbol-name name) ,sops-file-test-root) t)))
           (unwind-protect
               (progn ,@body)
             (ignore-errors "*sops-file-error*"))
           (delete-directory default-directory t))))))

(sops-file-test read-file ()
  (let ((relpath "read-file.enc.yaml")
        (contents "a: c\n"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert contents))
    (with-sops-identity-file 
      (format-find-file relpath 'sops-file))
    (should (equal (buffer-string) contents))
    (should (equal major-mode 'yaml-mode))))

(sops-file-test major-mode-respects-contents ()
  (let ((relpath "respects-contents"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert "#!/usr/bin/env sh"))
    (with-sops-identity-file
      (format-find-file relpath 'sops-file))
    (should (equal (buffer-string) "#!/usr/bin/env sh"))
    (should (equal major-mode 'sh-mode))))

(sops-file-test updates-are-saved ()
  (let ((relpath "updates-saved"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert "#!/usr/bin/env sh"))
    (with-sops-identity-file
      (save-current-buffer
        (format-find-file relpath 'sops-file)
        (while (search-forward "sh" nil t)
          (replace-match "awk" nil t))
        (save-buffer)
        (kill-buffer))
      (format-find-file relpath 'sops-file)
      (should (equal major-mode 'awk-mode)))))

(sops-file-test auto-mode-entry-point ()
  (let ((relpath "auto-mode-entry.enc.yaml"))
    (sops-file-test-setup-single-age-key)
    (with-yaml-mode-unavailable
      (with-sops-file-auto-mode
        (with-output-to-encrypted-sops-file relpath
          (insert "key: value\n"))
        (with-sops-identity-file
          (find-file relpath))
        (should (equal (buffer-string) "key: value\n"))
        (should (equal major-mode 'fundamental-mode))))))

(sops-file-test yaml-mode-entry-point ()
  (let ((relpath "yaml-mode-entry.enc.yaml"))
    (sops-file-test-setup-single-age-key)
    (with-sops-file-auto-mode
      (with-output-to-encrypted-sops-file relpath
        (insert "key: value\n"))
      (with-sops-identity-file
        (find-file relpath))
      (should (equal (buffer-string) "key: value\n"))
      (should (equal major-mode 'yaml-mode)))))

(sops-file-test passphrase-read-file ()
  (let ((relpath "passphrase-read-file.enc.yaml"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert "key: value\n"))
    (with-sops-identity-file 
      (with-encrypted-identity
        (with-passphrase-input sops-file-test-passphrase-key
          (format-find-file relpath 'sops-file))
        (should (equal (buffer-string) "key: value\n"))
        (should (equal major-mode 'yaml-mode))))))

(sops-file-test file-does-not-exist-is-silent ()
  (with-sops-file-auto-mode
    (find-file "ex.enc.yaml")
    (should (equal (buffer-string) ""))))

(sops-file-test sops-yaml-is-not-decrypted ()
  (with-sops-file-auto-mode
    (sops-file-test-setup-single-age-key)
    (find-file ".sops.yaml")
    (let ((retrieved (buffer-string))
          (on-disk (with-temp-buffer
                     (insert-file-contents ".sops.yaml")
                     (buffer-string))))
      (should (equal retrieved on-disk)))))

(sops-file-test cannot-decrypt-shows-error-in-sops-file-errors ()
  (let ((relpath "cannot-decrypt.enc.yaml"))
    (with-sops-file-auto-mode
      (sops-file-test-setup-single-age-key)
      (with-output-to-encrypted-sops-file relpath
        (insert "a: c"))
      (with-sops-identity-file
        (setenv "SOPS_AGE_KEY_FILE")
        (find-file relpath))))
  (with-current-buffer "*sops-file-error*"
    (let ((expected-failure "Failed to get the data key required to decrypt the SOPS file."))
      (should (equal
               (buffer-substring (point-min) (1+ (length expected-failure)))
               expected-failure)))))

(sops-file-test bad-passwd-shows-error-in-sops-file-errors ()
  (let ((relpath "bad-passwd.enc.yaml"))
    (with-sops-file-auto-mode
      (sops-file-test-setup-single-age-key)
      (with-output-to-encrypted-sops-file relpath
        (insert "a: b"))
      (with-sops-identity-file
        (with-encrypted-identity
          (with-passphrase-input "not-the-passphrase"
            (find-file relpath))))))
  (with-current-buffer "*sops-file-error*"
    (goto-char (point-min))
    (should (re-search-forward "incorrect passphrase"))))

(sops-file-test major-mode-in-filename-is-respected-after-decryption ()
  (let ((relpath "my-secret-package.enc.el"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath)
    (with-sops-file-auto-mode
      (with-sops-identity-file
        (find-file relpath)
        (should (equal major-mode 'emacs-lisp-mode))))))

(sops-file-test re-entering-does-not-redecode ()
  (let ((relpath "re-entering-no-decode.enc.yaml"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert "key: value\n"))
    (with-sops-file-auto-mode
      (with-sops-identity-file
        (find-file relpath)
        (format-find-file relpath 'sops-file)))
    (should (equal (buffer-string) "key: value\n"))))

(sops-file-test re-encode-allows-decode ()
  (let ((relpath "re-encode-allows-decode.enc.yaml"))
    (sops-file-test-setup-single-age-key)
    (with-output-to-encrypted-sops-file relpath
      (insert "key: value"))
    (with-sops-file-auto-mode
      (with-sops-identity-file
        (find-file-literally relpath)
        (format-decode-buffer 'sops-file)
        (should (equal (buffer-string) "key: value\n"))
        (format-encode-buffer 'sops-file)
        (should (not (equal (buffer-string) "key: value\n")))
        (format-decode-buffer 'sops-file)
        (should (equal (buffer-string) "key: value\n"))))))

;; (ert-deftest sops-file-test--file-creation ()
;;   (let ((relpath "non-existent-file.enc.yaml"))
;;     (with-sops-file-directory "file-creation"
;;       (with-sops-file-auto-mode
;;         )
;;       (format-find-file relpath 'sops-file))))

(provide 'sops-file-test)
