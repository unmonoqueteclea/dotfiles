;;; mono-base-definitions.el --- constants and variables -*- lexical-binding: t -*-

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
;;  Constants and variables that will be used in the rest of packages

;;; Code:
(setq debug-on-error nil)

;; Stop Emacs adding customised settings to init.el.
;; These configuration files should be the only place where
;; variables values are stored. We store custom values in a temp
;; file so changes are session local.
(setq custom-file (make-temp-file "emacs-custom"))
;; Set a high garbage collection threshold to achieve a
;; faster Emacs init
(setq gc-cons-threshold 1000000000)

(defconst mono-dir-vc (expand-file-name "~/vc")
  "Directory that contains all my version-controlled projects.")

(defconst mono-dir-sync (expand-file-name "~/Drive/")
  "Synchronized folder, mounted on init.")

(defconst mono-dir-sync-local (expand-file-name "~/DriveLocal/")
  "Local cache folder for `mono-dir-sync`, that is mounted")

(defconst mono-dir-org
  (expand-file-name "orgmode" mono-dir-sync)
  "Directory that contains main `org-mode` files.")

(defconst mono-dir-emacs
  (expand-file-name "~/.emacs.d")
  "Directory that contains Emacs configuration files.")

(defconst mono-dir-cache
  (expand-file-name "cache" mono-dir-emacs)
  "Directory where all non-permanent Emacs files are stored.")

(defconst mono-dir-notes
  (expand-file-name "denote" mono-dir-org)
  "Directory where notes are stored")

(defconst mono-dir-journal
  (expand-file-name "journal" mono-dir-org)
  "Directory where journal notes are stored")

(defconst mono-file-notes-buy
  (expand-file-name "20230313T082232--buy-compras__lifestyle.org" mono-dir-notes)
  "File where I store the things I buy")

(defconst mono-file-notes-books
  (expand-file-name "20230213T082456--libros__hobbies.org" mono-dir-notes)
  "File where I store books I read or I want to read")

(defconst mono-personal-email
  "pgonzalezcarrizo@gmail.com")

(defconst mono-work-email
  "gonzalez@bigml.com")

(provide 'mono-base-definitions)

;;; mono-base-definitions.el ends here
