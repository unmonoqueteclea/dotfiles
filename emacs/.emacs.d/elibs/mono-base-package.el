;;; mono-base-package.el --- Manage packages -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2024, 2025  Pablo González Carrizo

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
;;  The main tools we are using to manage packages are use-package and
;;  Straight.  init-files and version lockfiles should be the sole
;;  source of truth.
;;
;;  use-package is a macro that provides convenient syntactic sugar
;;  for many common tasks related to installing and configuring
;;  Emacs packages.
;;  See https://github.com/jwiegley/use-package
;;
;;  straight.el is the next-generation, purely functional package
;;  manager for the Emacs hacker.  With Straight, packages are cloned
;;  as Git repositories, so you can make changes to a package simply
;;  by editing its source code.  You must install Git in order to
;;  use straight.el.
;;
;;  Package versions are stored in file `straight-lockfile.el` so that
;;  configuration is fully reproducible.
;;
;;  You can update all packages doing:
;;  `straight-pull-all`
;;  Remember to update lockfile afterwards by doing
;;  `straight-freeze-versions`
;;  See https://github.com/raxod502/straight.el

;;; Code:

;; straight configuration from https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)  ;; to avoid lint error

;; configure straight to work with use-package without the needed to
;; add ':straight t'. It will be assumed unless you explicitly override
;; it with :straight nil
(straight-use-package 'use-package)
(require 'use-package)  ;; to avoid lint errors
(setq straight-use-package-by-default t)
;; avoid one problem while doing "require eglot"
;; see https://github.com/radian-software/straight.el/issues/551#issuecomment-667540139
(setq straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref jsonrpc external-completion))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; we want to defer load of all packages by default.  You can use
;; ':demand t' to force that a package is loaded from init.  If you've
;; set a package to be deferred, you then need to make sure there's a
;; way for it to get loaded when needed, for example by means of an
;; autoload (either provided by the package, or set up automatically
;; by use-package via :bind, or set up manually through use-package
;; via :commands) or by an explicit require in one of your custom
;; commands.
(setq use-package-always-defer t)

;; This library provides a linter for the metadata in Emacs Lisp files
;; which are intended to be packages. You can integrate it into your
;; build process.
(use-package package-lint)
;;(setq  package-lint-main-file "jira.el")

;; TODO Use a lockfile and upload it to the repo
;; TODO Need to configure ELPA, etc?

(provide 'mono-base-package)

;;; mono-base-package.el ends here
