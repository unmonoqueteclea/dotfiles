;;; mono-org.el --- org-mode configuration -*- lexical-binding: t -*-

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
;;  I use org-mode mainly to take notes (org-roam) to manage tasks and
;;  appointments (org-agenda), to manage spaced repetition flashcards
;;  (org-drill) and even for doing some literate programming.

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)


(use-package org
  :custom (org-modules '(org-habit))
  :config
  ;; configure org-crypt to encrypt some org-mode headers
  ;; you should import your public and secret gpg key
  ;; by doing 'gpg --import /path/to/your/gpg/key'
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; gpg key to use for encryption
  ;; either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "pgonzalezcarrizo@gmail.com"))

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


(setq org-confirm-babel-evaluate nil)
;; org-babel: languages that we want to be able to execute
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (restclient . t)
   )
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
  :hook (org-mode . org-modern-mode))

;; org-modern provides a clean and efficient org style. The blocks
;; (e.g. source, example) are particularly nice. But when org-indent
;; is enabled, the block "bracket", which uses the fringe area, is
;; disabled.  This small package reproduces the block styling of
;; org-modern when using org-indent:
(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent" :branch "main")
  :hook
  (org-indent-mode . org-modern-indent-mode))

;; Org-roam is a Roam replica
;; built on top of the all-powerful Org-mode.  Org-roam is a solution
;; for effortless non-hierarchical note-taking with Org-mode. With
;; Org-roam, notes flow naturally, making note-taking fun and
;; easy. Org-roam should also work as a plug-and-play solution for
;; anyone already using Org-mode for their personal wiki.
(use-package org-roam
  :demand t
  :functions org-roam-db-autosync-mode
  :custom
  (org-roam-completion-everywhere t)
   (org-roam-directory  (concat mono-dir-org "/roam"))
  (org-roam-db-location (concat mono-dir-org "/roam/org-roam.db"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n a" . org-roam-alias-add)
         ("C-c n c" . org-roam-capture)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n l" . org-roam-buffer-toggle)
         ;; to create a org-roam node from a heading
         ("C-c n n" . org-id-get-create))
  :config
  ;; from org-roam config. Show org-roam buffer as a side-window
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
	       ;; Let use move the cursor to side-window
               (window-parameters . ((no-other-window . nil)
                                     (no-delete-other-windows . t)))))
  (setq org-roam-db-node-include-function
	;; org-roam should ignore :drill headers
	(lambda ()
          (not (member "drill" (org-get-tags)))))
  (require 'org-roam-protocol))

;; org-roam-ui: a graphical frontend for your org-roam Zettelkasten
;; See https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui
    :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; from https://github.com/org-roam/org-roam/issues/507
(defun tim/org-roam-buffer-show (_)
  "Always open side buffer when open an org-roam buffer."
  (if (and
       ;; don't do anything if we're in the minibuffer
       (not (minibufferp))
       ;; show org-roam buffer iff the current buffer has a org-roam file
       (xor (org-roam-file-p) (eq 'visible (org-roam-buffer--visibility))))
      (org-roam-buffer-toggle)))
(add-hook 'window-buffer-change-functions 'tim/org-roam-buffer-show)

;; Org-Drill is an extension for Org mode. Org-Drill uses a spaced
;; repetition algorithm to conduct interactive "drill sessions", using
;; org files as sources of facts to be memorised.
(use-package org-drill
  :config (setq org-drill-learn-fraction 0.25))

;; this package is needed to perform syntax highlight in
;; code fragments exported from org-files into html files
(use-package htmlize)

;; open org-mode links (including org-roam ones) in the same window
(add-to-list 'org-link-frame-setup '(file . find-file))

(provide 'mono-org)

;;; mono-org.el ends here
