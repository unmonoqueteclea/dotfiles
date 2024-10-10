;;; mono-org.el --- org-mode configuration -*- lexical-binding: t -*-

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
;;  I use org-mode mainly to take notes (see mono-notes) to manage tasks and
;;  appointments (org-agenda), to manage spaced repetition flashcards
;;  (org-drill) and even for doing some literate programming.

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)


(use-package org
  :custom (org-modules '(org-habit))
  :config
  ;; configure org-crypt to encrypt some org-mode headers you should
  ;; import your public and secret gpg key by doing 'gpg --import
  ;; /path/to/your/gpg/key'

  ;;WARNING: if it suddenly stops working, you may need to update your key
  (require 'org-crypt)
  (require 'org-archive)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; gpg key to use for encryption
  ;; either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "pgonzalezcarrizo@gmail.com")
  ;; both the state changes and the timer logs are placed inside the
  ;; drawer
  (setq org-log-into-drawer t)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)"))))

(require 'org-indent)
(require 'org-element)
(require 'org-num)

(setq org-src-fontify-natively t
      org-indent-mode t
      org-startup-indented t
      org-auto-align-tags nil
      org-tags-column 0
      org-fold-catch-invisible-edits 'show-and-error
      org-special-ctrl-a/e t
      org-insert-heading-respect-content t
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-ellipsis "…"
      ;; open links with Return key
      org-return-follows-link t)

;; the combination of `restclient` and `ob-restclient` allows
;; me to create org-mode files with some API requests
;; Like Postman, but within emacs
(use-package restclient)
(use-package ob-restclient)

(setq org-duration-format (quote h:mm))
(setq org-confirm-babel-evaluate nil)
;; org-babel: languages that we want to be able to execute
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t) (python . t) (restclient . t) (sql . t))
 )

;; this package implements a “modern” style for your Org buffers using
;; font locking and text properties. The package styles headlines,
;; keywords, tables and source blocks. The styling is configurable,
;; you can enable, disable or modify the style of each syntax element
;; individually via the org-modern customization group.
;; See https://github.com/minad/org-modern
(use-package org-modern
  :custom
  (org-modern-hide-stars nil) ; adds extra indentation
  :hook (org-mode . org-modern-mode)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;; org-modern provides a clean and efficient org style. The blocks
;; (e.g. source, example) are particularly nice. But when org-indent
;; is enabled, the block "bracket", which uses the fringe area, is
;; disabled.  This small package reproduces the block styling of
;; org-modern when using org-indent:
(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent" :branch "main")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; Org-Drill is an extension for Org mode. Org-Drill uses a spaced
;; repetition algorithm to conduct interactive "drill sessions", using
;; org files as sources of facts to be memorised.
(use-package org-drill
  :config (setq org-drill-learn-fraction 0.25))

;; this package is needed to perform syntax highlight in
;; code fragments exported from org-files into html files
(use-package htmlize)

;; this package allows running gnuplot files from within the GNU Emacs
;; editor (you will also need the gnuplot system package)
(use-package gnuplot)

(use-package org-tree-slide
  :config (setq org-tree-slide-cursor-init nil))

;; configure safe commands that can be executed from org-links without confirmation
(let ((safe-commands '(org-reset-checkbox-state-subtree)))
  (setq org-link-elisp-skip-confirm-regexp
        (mapconcat #'symbol-name safe-commands "\\|")))

;; open org-mode links in the same window
;;(add-to-list 'org-link-frame-setup '(file . find-file))


(provide 'mono-org)

;;; mono-org.el ends here
