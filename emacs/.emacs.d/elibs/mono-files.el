;;; mono-files.el --- filesystem related operations -*- lexical-binding: t -*-

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
;;


;;; Code:
(require 'mono-base-package)
(require 'mono-base-theme) ;; all-the-icons is defined there

;; show icons in dired items
;; See https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; this package adds a single command dired-rsync which allows the
;; user to copy marked files in a dired buffer via rsync.
;; see https://github.com/stsquad/dired-rsync
(use-package dired-rsync
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

;; always follow links to git files
(setq vc-follow-symlinks t)

;; omit some kind of special files in dired
(require 'dired-x)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      "^.DS_Store$\\|^.git$\\|.egg-info$\\|.pytest_cache$\\|__pycache__$\\|Icon\015$\\|.mypy_cache$")

;; a: show hidden fles
;; l: list files in long format
;; h: list the size in human readable format
(setq dired-listing-switches "-alh")

(provide 'mono-files)

;;; mono-files.el ends here
