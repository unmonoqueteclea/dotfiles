;;; mono-projects.el --- handle projects workspaces -*- lexical-binding: t -*-

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
;;  Last full review: 2022-12-28
;;
;;  We use Emacs tabs and project.el to separate different workspaces.
;;  Using [C-c w] prefix.

;;; Code:
(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-org)
(require 'mono-complete)

;; support tabs, change between them with C-RET
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show nil)
(global-set-key (kbd "C-<return>") 'tab-switch)
(define-key org-mode-map (kbd "C-<return>") 'tab-switch)

(use-package consult-project-extra
  :bind
  (("C-c p" . consult-project-extra-find)))

;; public function to add a new project
(defun mono/new-project-tab ()
  "Open a project in a new tab."
  (interactive)
  (other-tab-prefix)
  (let* ((dir (project-prompt-project-dir)))
    (project-switch-project dir)
    (tab-rename (consult--project-name dir))))

(defun mono/new-mail-tab ()
  "Open a new tab with mail."
  (interactive)
  (tab-new-to)
  (mu4e)
  (tab-rename "mail")
  (mu4e-update-mail-and-index t))

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
(global-set-key (kbd "C-c w a") 'mono/new-agenda-tab)
(global-set-key (kbd "C-c w d") 'mono/new-docker-tab)
(global-set-key (kbd "C-c w D") 'mono/new-draft-tab)
(global-set-key (kbd "C-c w m") 'mono/new-mail-tab)
;; you have also a keybind in "C-c n" to open denote within a new tab

(provide 'mono-projects)

;;; mono-projects.el ends here
