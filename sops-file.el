;;; sops-file.el --- A major mode for sops encrypted files -*- lexical-binding: t; -*-

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

(defgroup sops-file nil "Transparently manipulate SOPS files" :prefix 'sops-file :group 'convenience)

(defcustom sops-file-executable "sops"
  "Path to the sops executable."
  :group 'sops-file
  :type 'string)

(defcustom sops-file-decrypt-args
  `("-d")
  "Decrypt arguments for sops."
  :group 'sops-file
  :type '(repeat string))



;; (put 'insert-file-contents 'epa-file 'epa-file-insert-file-contents)

;; (put 'write-region 'sops-file 'sops-file-write-region)


(provide 'sops-file)
;;; sops-file.el ends here
