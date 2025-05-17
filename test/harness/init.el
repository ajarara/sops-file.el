;; -*- lexical-binding: t -*-

(require 'cl-lib)
(cl-assert load-true-file-name t)
(let ((harness-dir (file-name-directory load-true-file-name)))
  (setq user-emacs-directory harness-dir)
  (add-to-list 'load-path (expand-file-name "../.." harness-dir)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'yaml-mode)
(require 'yaml-mode)

(require 'sops-file)

