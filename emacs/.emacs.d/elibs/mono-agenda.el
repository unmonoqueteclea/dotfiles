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

;; Last full review: 2025-05-21

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

(defconst mono-agenda-routines
  (expand-file-name "routines.org" mono-dir-agenda)
  "Routines agenda file.")

(defconst mono-agenda-tasks
  (expand-file-name "tasks.org" mono-dir-agenda)
  "Tasks file.")

(setq org-agenda-time-grid nil)
(setq org-agenda-files (list mono-agenda-routines mono-agenda-tasks))
(setq org-agenda-overriding-header "📅 Agenda \n")
(setq org-agenda-current-time-string "---------------------------------------------------- now")
(setq org-agenda-start-on-weekday 1)
(customize-set-variable 'org-agenda-prefix-format "%-2i %?-12t%s")  ;;"  %?-2i %t "
(customize-set-variable 'org-agenda-todo-keyword-format "%-10s")
(customize-set-variable 'org-agenda-scheduled-leaders '("🕐    " "🕐 %2dd"))
(customize-set-variable 'org-agenda-deadline-leaders '("❗" "❗ +%2dd" "❗ -%3dd"))

(setq org-agenda-category-icon-alist
   `(("routines" ,(list (all-the-icons-faicon "spinner" :height 0.8)) nil nil :ascent center)
     ("tasks" ,(list (all-the-icons-faicon "check-square" :v-adjust 0.005)) nil nil :ascent center)))

(add-to-list
 'org-agenda-custom-commands
 '("d" "Today's agenda"
   ((agenda ""
            ((org-agenda-span 'day)
             (org-agenda-sorting-strategy
              '((agenda time-up priority-down category-keep))))))))

(setq org-capture-templates
      '(("w" "👷 Trabajo" entry (file+headline mono-agenda-tasks "Work")
	 "* TODO %^{task} \nSCHEDULED: <%<%Y-%m-%d %a>>"
	 :empty-lines 1)
	("t" "✏️ Tasks" entry (file+headline mono-agenda-tasks "Inbox")
	 "* TODO %^{text} \nSCHEDULED: <%<%Y-%m-%d %a>>"
	 :empty-lines 1)
	("b" "🧺 Compra" entry (file mono-file-notes-buy)
	 "* %^{item} \n:PROPERTIES:\n:url: [[%^{link-url}][%^{link-description}]]\n:date: %^{date?}\n:price: %^{price?}\n:notes: %^{notes?}\n:END:\n "
	 :empty-lines 1)
	("B" "📚 Libro" entry (file mono-file-notes-books)
	 "* %^{item} :pending: \n:PROPERTIES:\n:year: %^{year when finished reading}\n:author: %^{author?}\n:rating: %^{rating (1-5)?}\n:END:\n "
	 :empty-lines 1)))

(provide 'mono-agenda)
;;; mono-agenda.el ends here
