;;; mono-agenda.el --- agenda and tasks management -*- lexical-binding: t -*-

;; copyright (C) 2022,2023,2024  Pablo González Carrizo

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

(setq org-habit-graph-column 100
      org-habit-show-all-today nil
      org-habit-show-habits-only-for-today nil
      org-habit-preceding-days 14
      org-habit-following-days 14
      org-agenda-tags-column -160)

(setq org-agenda-overriding-header "⚡ Agenda")

;; add a separator line between each day
(setq org-agenda-format-date
      (lambda (date)
	(concat (make-string 80 9472)
                "\n"
                (org-agenda-format-date-aligned date))))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t % s  %?-5e")
	(todo . " %i %-12:c %?5e ")
	(tags . " %i %-12:c")
	(search . " %i %-12:c")))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; assign icons to categories
(setq org-agenda-category-icon-alist
      `(("Trabajo" ,(list (all-the-icons-material "work")) nil nil :ascent center)
        ("Rutina" ,(list (all-the-icons-material "watch")) nil nil :ascent center)
	("Home" ,(list (all-the-icons-material "home")) nil nil :ascent center)
	("Social" ,(list (all-the-icons-material "person")) nil nil :ascent center)
	("Inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
	("Emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)
	("Kaizen" ,(list (all-the-icons-faicon "percent")) nil nil :ascent center)
	("Travel" ,(list (all-the-icons-faicon "plane")) nil nil :ascent center)
        ("Buy" ,(list (all-the-icons-material "shopping_basket")) nil nil :ascent center)
	("Finances" ,(list (all-the-icons-faicon "money")) nil nil :ascent center)
	("Blog" ,(list (all-the-icons-faicon "pencil-square")) nil nil :ascent center)
	("Projects" ,(list (all-the-icons-alltheicon "script")) nil nil :ascent center)))

;; this package lets you “supercharge” your Org daily/weekly
;; agenda. The idea is to group items into sections, rather than
;; having them all in one big list.
;; See https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :demand t
  :config (org-super-agenda-mode))

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
	  (:name "⚠️ Debería haber terminado..."
	    :scheduled past ;; show past scheduled tasks
	    :deadline past ;; show past deadline tasks
	    :order 1)
	  (:name "📅 Tareas para hoy"
           ;; only show tasks from category trabajo
           ;; :discard (:not (:category ("trabajo")))
           :date today ;; match items with today’s date
           :order 2)))))
     (alltodo
      ""
      ((org-agenda-overriding-header "✅ Otras tareas")
       (org-super-agenda-groups
        '((:name "🔫 LFX":tag "lfx") ;; ensrue these tasks are shown before the rest
          (:name "Otras " :auto-tags t :discard (:not (:tag ("Rutina"))))))))))))

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

;; see https://github.com/ml729/org-habit-stats/
(use-package org-habit-stats
  :straight (org-habit-stats :type git :host github :repo "ml729/org-habit-stats"))

;; open habit stats view for agenda items
(add-hook
 'org-agenda-mode-hook
 (lambda ()
   (local-set-key (kbd "H") 'org-habit-stats-view-habit-at-point-agenda)))

(provide 'mono-agenda)
;;; mono-agenda.el ends here
