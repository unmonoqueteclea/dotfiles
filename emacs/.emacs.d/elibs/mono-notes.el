;;; mono-notes.el --- take notes within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022  Pablo González Carrizo

;; Author: Pablo González Carrizo <pgonzalezcarrizo@gmail.com>

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

;;; Code:


(defun mono/open-denote-directory ()
  "Open the main denote directory."
  (interactive)
  (tab-new)
  (tab-rename "notes")
  (find-file denote-directory))

(defun mono/search-in-notes ()
  "Execute ripgrep within the denote directory."
  (interactive)
  (find-file denote-directory)
  (consult-ripgrep))

(use-package denote
  :demand t
  :config
  (setq denote-directory mono-dir-notes)
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  :bind (("C-c n n" . denote)
	 ("C-c n o" . mono/open-denote-directory)
	 ("C-c n i" . denote-link)
	 ("C-c n s" . mono/search-in-notes)))

(provide 'mono-notes)
;;; mono-notes.el ends here
