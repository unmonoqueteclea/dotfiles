;;; mono-agenda.el --- agenda and tasks management -*- lexical-binding: t -*-

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


;;; Commentary:
;;

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
  (expand-file-name "work.org" mono-dir-agenda)
  "Work agenda file.")

(defconst mono-agenda-inbox
  (expand-file-name "inbox.org" mono-dir-agenda)
  "Inbox agenda file.")

(defconst calendar-bigml
  (expand-file-name "cal-bigml.org" mono-dir-agenda))

(defconst calendar-google
  (expand-file-name "cal-google.org" mono-dir-agenda))

;; everything in the agenda folder should be shown in the agenda
(setq org-agenda-files `(,mono-dir-agenda))

(setq org-habit-graph-column 80
      org-habit-show-habits-only-for-today nil)

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
      `(("work" ,(list (all-the-icons-material "work")) nil nil :ascent center)
        ("routine" ,(list (all-the-icons-material "watch")) nil nil :ascent center)
	("home" ,(list (all-the-icons-material "home")) nil nil :ascent center)
	("social" ,(list (all-the-icons-material "person")) nil nil :ascent center)
	("inbox" ,(list (all-the-icons-faicon "inbox")) nil nil :ascent center)
	("emacs" ,(list (all-the-icons-fileicon "emacs")) nil nil :ascent center)
	("kaizen" ,(list (all-the-icons-faicon "percent")) nil nil :ascent center)
	("travel" ,(list (all-the-icons-faicon "plane")) nil nil :ascent center)
	("finances" ,(list (all-the-icons-faicon "money")) nil nil :ascent center)
	("calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
	("blog" ,(list (all-the-icons-faicon "pencil-square")) nil nil :ascent center)))

;; this package lets you “supercharge” your Org daily/weekly
;; agenda. The idea is to group items into sections, rather than
;; having them all in one big list.
;; See https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :demand t
  :config (org-super-agenda-mode))

(setq
 org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
 org-agenda-compact-blocks nil
 org-agenda-window-setup 'current-window
 org-agenda-start-on-weekday 1
 org-deadline-warning-days 7
 org-agenda-custom-commands
 '(
   ("w" "Work agenda view"
    ((agenda
      ""
      ((org-agenda-span 1)  ;; show 1 day by default
       (org-super-agenda-groups
	'((:name "Habit" :habit)
	  (:name "Should have done..."
	    ;; only show tasks from category work or calendar
	    :discard (:not (:category ("work" "calendar")))
	    ;; show past scheduled tasks
	    :scheduled past
	    ;; show past deadline tasks
	    :deadline past
	    :order 1)
	  (:name "Today's work tasks"
           ;; only show tasks from category work or calendar
           :discard (:not (:category ("work" "calendar")))
           ;; match items with today’s date
           :date today
           :order 2)))))
     (alltodo
      ""
      ((org-agenda-overriding-header "Other work tasks")
       (org-super-agenda-groups
         ;; only show tasks from category work or calendar
	'((:discard (:not (:category ("work" "calendar"))))
	  (:auto-tags t))
	  )))
     ))

   ("p" "Personal agenda view"
    ((agenda
      ""
      ((org-agenda-span 1)  ;; show 1 day by default
       (org-super-agenda-groups
	'((:name "Habit" :habit)
	  (:name "Should have done..."
	    ;; only show tasks from category work or calendar
	    :discard (:category ("work"))
	    ;; show past scheduled tasks
	    :scheduled past
	    ;; show past deadline tasks
	    :deadline past
	    :order 1)
	  (:name "Today's work"
           ;; only show tasks from category work or calendar
           :discard (:category ("work"))
           ;; match items with today’s date
           :date today
           :order 2)))))
     (alltodo
      ""
      ((org-agenda-overriding-header "Other tasks")
       (org-super-agenda-groups
         ;; only show tasks from category work or calendar
	'((:discard (:category ("work")))
	  (:auto-category t))
	  )))
     ))))

;; synchronize org files with google calendar
(use-package org-gcal
 :demand t
 :after org
 :config
 (setq org-gcal-client-id (getenv "GOOGLE_CLIENT_ID")
	org-gcal-client-secret (getenv "GOOGLE_CLIENT_SECRET")
	org-gcal-fetch-file-alist
	`((,mono-work-email . ,calendar-bigml)
	  (,mono-personal-email . ,calendar-google))
	org-gcal-recurring-events-mode "nested"))


(setq org-capture-templates
      '(("w" "Work task" entry (file+headline mono-agenda-work "Inbox")
	 "* TODO %^{task}
 :PROPERTIES:
 :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
 :END:"
	 :empty-lines 1)
	("p" "Personal task" entry (file mono-agenda-inbox)
	 "* TODO %^{task}"
	 :empty-lines 1)))
 
(provide 'mono-agenda)
;;; mono-agenda.el ends here
