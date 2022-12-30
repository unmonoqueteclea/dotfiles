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
(require 'mono-buffer) ;; diminish definition

;; show icons in dired items
;; See https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :diminish all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; always follow links to git files
(setq vc-follow-symlinks t)

;; omit some kind of special files in dired
(require 'dired-x)
(add-hook 'dired-mode-hook #'dired-omit-mode)
(setq dired-omit-files
      "^.DS_Store$\\|^.git$\\|.egg-info$\\|.pytest_cache$\\|__pycache__$\\|Icon\015$\\|.mypy_cache$")

;; ls --help to show switches
(setq dired-listing-switches "-alGh")

(provide 'mono-files)

;;; mono-files.el ends here
