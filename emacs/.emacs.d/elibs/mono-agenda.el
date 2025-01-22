;;; mono-agenda.el --- agenda and tasks management -*- lexical-binding: t -*-

;; copyright (C) 2022,2023,2024,2025  Pablo González Carrizo

;; Author: Pablo González Carrizo <pgonzalezcarrizo@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;;; Commentary:
;;  In the main agenda files, I am using an ARCHIVE entry for each
;;  header.  To send a finished item to ARCHIVE, use function
;;  =org-archive-to-archive-sibling=

;; Last full review: 2024-11-03

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-org)
(require 'mono-base-theme)

(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c c")  'org-capture)

(defconst mono-dir-agenda
  (expand-file-name "agenda" mono-dir-org)
  "Directory that contains main agenda files.")

(defconst mono-agenda-work
  (expand-file-name "trabajo.org" mono-dir-agenda)
  "Work agenda file.")

(defconst mono-agenda-inbox
  (expand-file-name "inbox.org" mono-dir-agenda)
  "Inbox agenda file.")

(defconst mono-agenda-projects
  (expand-file-name "projects.org" mono-dir-agenda)
  "Projects file.")

;; everything in the agenda folder should be shown in the agenda
(setq org-agenda-files `(,mono-dir-agenda))

(setq org-habit-graph-column 120
      org-habit-show-habits-only-for-today nil
      org-habit-preceding-days 21
      org-habit-following-days 3
      org-agenda-tags-column -160)

(setq org-agenda-overriding-header "⚡ Agenda de hoy \n")

(setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))

;; assign icons to categories
(setq org-agenda-category-icon-alist
      `(("Trabajo" ,(list (all-the-icons-material "work")) nil nil :ascent center)
        ("Rutina" ,(list (all-the-icons-material "watch")) nil nil :ascent center)
	("Home" ,(list (all-the-icons-faicon "key")) nil nil :ascent center)
	("Social" ,(list (all-the-icons-material "person")) nil nil :ascent center)
	("Inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
	("Emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)
	("Kaizen" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
	("Travel" ,(list (all-the-icons-faicon "paper-plane")) nil nil :ascent center)
        ("Buy" ,(list (all-the-icons-material "shopping_basket")) nil nil :ascent center)
	("Finances" ,(list (all-the-icons-faicon "money")) nil nil :ascent center)
	("Blog" ,(list (all-the-icons-faicon "pencil-square")) nil nil :ascent center)
	("Projects" ,(list (all-the-icons-faicon "th")) nil nil :ascent center)))

;; this package lets you “supercharge” your Org daily/weekly
;; agenda. The idea is to group items into sections, rather than
;; having them all in one big list.
;; See https://github.com/alphapapa/org-super-agenda
(use-package
  org-super-agenda
  :demand t
  :config
  (org-super-agenda-mode)
  (setq
   org-agenda-time-grid nil
   org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
   org-agenda-compact-blocks nil
   org-agenda-window-setup 'current-window
   org-agenda-start-on-weekday 1
   org-deadline-warning-days 7
   org-agenda-custom-commands
   '(
     ("w" "🖥️ Trabajo"
      ((agenda
	""
	((org-agenda-span 1)  ;; show 1 day by default
	 (org-super-agenda-groups
	  '((:name "➰ Rutinas" :habit)
	    (:name "⚠️ Debería haber terminado..." :scheduled past :deadline past :order 1)
	    (:name "📅 Tareas para hoy" :date today :order 2)))))
       (alltodo
	""
	((org-agenda-overriding-header "✅ Otras tareas")
	 (org-super-agenda-groups '((:name "Otras")))))))

     ("o" "☑️ Otras"
      ((agenda
	""
	((org-agenda-span 1)  ;; show 1 day by default
	 (org-super-agenda-groups
	  '((:name "➰ Rutinas" :habit)
	    (:name "⚠️ Debería haber terminado..." :discard (:category ("Trabajo")) :scheduled past :deadline past :order 1)
	    (:name "📅 Tareas para hoy" :discard (:category ("Trabajo")) :date today :order 2)))))
       (alltodo
	""
	((org-agenda-overriding-header "✅ Otras tareas")
	 (org-super-agenda-groups
          '((:name "Otras" :discard (:and (:category ("Trabajo")) :habit)))))))))))



(setq org-capture-templates
      '(("w" "👷 Trabajo" entry (file+headline mono-agenda-work "Inbox")
	 "* TODO %^{task}"
	 :empty-lines 1)
	("p" "🧑 Tarea personal" entry (file mono-agenda-inbox)
	 "* TODO %^{task}"
	 :empty-lines 1)
	("x" "💰 Proyecto personal" entry (file+headline mono-agenda-projects "tasks")
	 "* TODO %^{text} %^g \nSCHEDULED: <%<%Y-%m-%d %a>>"
	 :empty-lines 1)
	("b" "🧺 Compra" entry (file mono-file-notes-buy)
	 "* %^{item} \n:PROPERTIES:\n:url: [[%^{link-url}][%^{link-description}]]\n:date: %^{date?}\n:price: %^{price?}\n:notes: %^{notes?}\n:END:\n "
	 :empty-lines 1)
	("B" "📚 Libro" entry (file mono-file-notes-books)
	 "* %^{item} :pending: \n:PROPERTIES:\n:year: %^{year when finished reading}\n:author: %^{author?}\n:rating: %^{rating (1-5)?}\n:END:\n "
	 :empty-lines 1)))

(provide 'mono-agenda)
;;; mono-agenda.el ends here
