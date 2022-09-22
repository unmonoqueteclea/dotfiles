;;; init.el --- Where everything begins -*- lexical-binding: t -*-

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
;;  This is the main init file that requires all the needed
;;  subpackages.
;;
;;  The configuration is splitted into several packages, all of them
;;  using the 'mono' preffix.
;;
;;  This file should only contain the needed 'require' instructions
;;  and some documentation.

;;; Code:

;; Add elibs folder to load-path to ensure we are able to load all the
;; needed packages
(add-to-list 'load-path "~/.emacs.d/elibs/")
;; always prefer new versions of .el files
;; avoid problems with emacs using old compiled .elc files
(setq load-prefer-newer t)

(require 'mono-base-definitions)
(require 'mono-base-session)
(require 'mono-base-package)
(require 'mono-base-theme)
(require 'mono-files)
(require 'mono-help)
(require 'mono-buffer)
(require 'mono-complete)
(require 'mono-org)
(require 'mono-docs)
(require 'mono-tools-terminal)
(require 'mono-agenda)
(require 'mono-projects)
(require 'mono-dev-tools)
(require 'mono-dev-web)
(require 'mono-dev-python)
(require 'mono-blog)
(require 'mono-tools-browser)
(require 'mono-tools-feeds)
(require 'mono-keyboard)

(provide 'init)

;;; init.el ends here
