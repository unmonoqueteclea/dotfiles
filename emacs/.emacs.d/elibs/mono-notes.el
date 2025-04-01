;;; mono-notes.el --- take notes within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2024  Pablo González Carrizo

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

(require 'mono-base-definitions)

(use-package denote
  :demand t
  :config
  (setq denote-directory mono-dir-notes)
  (add-hook 'dired-mode-hook #'denote-dired-mode))

(use-package denote-journal  :straight
  (denote-joutnal :type git :host github :repo "protesilaos/denote-journal")
  :config
  (setq denote-journal-directory mono-dir-journal)
  (setq denote-date-prompt-use-org-read-date t))

(provide 'mono-notes)
;;; mono-notes.el ends here
