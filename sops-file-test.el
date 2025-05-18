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

(require 'ert)
(require 'yaml-mode)
(require 'sops-file)

(ert-deftest sops-file-test--read-file ()
  (let* ((age-keygen-pair
          (with-temp-buffer
            (call-process "age-keygen" nil (current-buffer))
            (goto-char (point-min))
            (let (public-key private-key)
              (when (re-search-forward "^# public key: \\(.+\\)$" nil t)
                (setq public-key (match-string 1)))
              (when (re-search-forward "^\\(AGE-SECRET-KEY-.*\\)$" nil t)
                (setq private-key (match-string 1)))
              (list public-key private-key))))
         (.sops.yaml-contents
          (format "
creation_rules:
  - path_regex: my-secrets.enc.yaml
    age: %s
" (car age-keygen-pair)))
         (default-directory (make-temp-file "sops-file-test--read-file" t))
         (identity-file (with-temp-file "identity.txt"
                          (insert (cadr age-keygen-pair))
                          (expand-file-name "identity.txt")))
         (my-secrets.enc.yaml-contents "my-key: my-value\n")
         (_
          (with-temp-buffer
            (insert .sops.yaml-contents)
            (write-file ".sops.yaml")))
         (_
          (with-temp-buffer
            (insert my-secrets.enc.yaml-contents)
            (write-file "my-secrets.enc.yaml")))
         (secret-file "my-secrets.enc.yaml")
         (result-code
          (call-process "sops" nil nil nil "encrypt" "-i" secret-file)))
    (should (equal 0 result-code))

    (setenv "SOPS_AGE_KEY_FILE" identity-file)
    (format-find-file secret-file 'sops-file)
    (should (equal (buffer-string) my-secrets.enc.yaml-contents))
    (should (equal 'yaml-mode major-mode))))


(provide 'sops-file-test)
