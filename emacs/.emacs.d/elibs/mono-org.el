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
  ;; WARNING: if it suddenly stops working, you may need to update your key
  (require 'org-crypt)
  (require 'org-archive)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; gpg key to use for encryption either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "pgonzalezcarrizo@gmail.com")
  ;; both the state changes and the timer logs are placed inside the  drawer
  (setq org-log-done nil)
  (setq org-log-done-with-time nil)
  (setq org-log-into-drawer nil)
  (setq org-log-repeat nil))

(require 'org-indent)
(require 'org-element)
(setq org-element-use-cache nil)

;; configuration recommended by org-modern
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
  (setq org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("⏵" . "⏷") ("▹" . "▿") ("▸" . "▾")))
  (setq org-modern-todo-faces '(("WAITING" . (:inherit org-modern-todo :foreground "orange")))))

;; this package is needed to perform syntax highlight in
;; code fragments exported from org-files into html files
(use-package htmlize)

;; this package allows running gnuplot files from within the GNU Emacs
;; editor (you will also need the gnuplot system package)
(use-package gnuplot)

(use-package org-tree-slide :config (setq org-tree-slide-cursor-init nil))

;; open org-mode links in the same window
(add-to-list 'org-link-frame-setup '(file . find-file))


;; multiple lines emphasis:
;; https://emacs.stackexchange.com/questions/18101/org-mode-multi-line-emphasis-and-bold
(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components))

(provide 'mono-org)

;;; mono-org.el ends here
