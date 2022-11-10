;;; mono-slides.el --- write slides from emacs -*- lexical-binding: t -*-

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
;;  This module contains all the configuration needed to write nice
;;  slides from org-mode using reveal.js

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-org)

;; mermaid diagrams are compatible with github's markdown renderer and
;; they are a very simple way to add diagrams to my org-mode files.
;; Generated diagrams will be shown in the exported revealjs slides
(use-package mermaid-mode)
;; I need to use my own fork until the PR that fixes CLI arguments is merged
(use-package ob-mermaid  :straight
  (ob-mermaid :type git :host github :repo "unmonoqueteclea/ob-mermaid" :branch "master"))

(org-babel-do-load-languages
 'org-babel-load-languages
 (append org-babel-load-languages
         '((mermaid     . t))))

;;export org-mode documents to reveal.js slides
;; See https://github.com/yjwen/org-reveal
(use-package ox-reveal
  :demand t
  :config
  (setq org-reveal-root
	(concat "file://" (file-name-concat mono-dir-vc "reveal.js"))))

(provide 'mono-slides)
;;; mono-slides.el ends here
