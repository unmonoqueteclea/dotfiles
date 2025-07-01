;;; mono-projects.el --- handle projects workspaces -*- lexical-binding: t -*-

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
;;  Last full review: 2022-12-28
;;
;;  We use Emacs tabs and project.el to separate different workspaces.
;;  Using [C-c w] prefix.

;;; Code:
(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-org)
(require 'mono-complete)
(require 'mono-agenda)
(require 'mono-secret)

;; config how project.el finds a project
(setq project-vc-extra-root-markers '("pyproject.toml" ".python-version"))

;; support tabs, change between them with C-RET
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show nil)
(global-set-key (kbd "C-<return>") 'tab-switch)
(define-key org-mode-map (kbd "C-<return>") 'tab-switch)

(use-package consult-project-extra
  :bind (("C-c p" . consult-project-extra-find)))

;; public function to add a new project
(defun mono/new-project-tab ()
  "Open a project in a new tab."
  (interactive)
  (other-tab-prefix)
  (let* ((dir (project-prompt-project-dir)))
    (project-switch-project dir)
    (tab-rename (consult--project-name dir))))


(defun mono/new-docker-tab ()
  "Open a new tab with docker containers."
  (interactive)
  (tab-new-to)
  (docker-containers)
  (tab-rename "docker"))

(defun mono/new-agenda-tab ()
  "Open a new tab with agenda."
  (interactive)
  (tab-new-to)
  (org-agenda)
  (tab-rename "agenda"))

(defun mono/new-draft-tab ()
  "Open a draft tab."
  (interactive)
  (tab-new-to)
  (switch-to-buffer "draft")
  (tab-rename "draft"))

(global-set-key (kbd "C-c w r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c w k") 'tab-bar-close-tab)
(global-set-key (kbd "C-c w o") 'mono/new-project-tab)
(global-set-key (kbd "C-c w n") 'denote-open-or-create)
(global-set-key (kbd "C-c w a") 'mono/new-agenda-tab)
(global-set-key (kbd "C-c w d") 'mono/new-docker-tab)
(global-set-key (kbd "C-c w D") 'mono/new-draft-tab)
;; c-c w n is used in denote (see mono-notes.el)

;; for some languages, try to use tree-sitter
(use-package treesit-auto
  :demand t
  :custom (treesit-auto-langs '(python javascript bash clojure markdown toml vue yaml))
  :config
  (global-treesit-auto-mode))

(use-package jira
  :straight (:host github :repo "unmonoqueteclea/jira.el")
  :demand t
  :config
  (setq jira-debug nil)

  (defface jira-face-pr
  '((t (:foreground "white" :background "orange" :weight bold)))
  "Face for pull-request status."
  :group 'jira)

  (defface jira-face-blocked
  '((t (:foreground "white" :background "red" :weight bold)))
  "Face for blocked status."
  :group 'jira)

  (setq jira-status-faces '(("Pull-request" . jira-face-pr)
			    ("Blocked" . jira-face-blocked)))

  (setq jira-issues-fields-extra
	'((:center . ((:path . (fields (custom "Cost center")))
                      (:columns . 10)
                      (:name . "Center")))))
  (setq jira-base-url secret-jira-base-url))

(provide 'mono-projects)

;;; mono-projects.el ends here
