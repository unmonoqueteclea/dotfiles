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
;;  We use Emacs tabs and projectile to separate different workspaces.
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

;; projectile is a project interaction library for Emacs.
;; use 'projectile-add-known-project' to add a new project
;; https://github.com/bbatsov/projectile
(use-package projectile
  :demand t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c P" . projectile-command-map))
  :config
  (setq projectile-project-search-path `(,mono-dir-vc))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function
	(lambda (_) (projectile-project-root))))

;;  a package to incorporate projectile into consult.
;;  This allows to choose a project, when none is selected or
;;  choose a project buffer/file.
;;  See https://gitlab.com/OlMon/consult-projectile
(use-package consult-projectile
  :straight
  (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :commands consult-projectile
  :bind
  (("C-c p" . consult-projectile)))

(defun mono/new-project-tab ()
  "Open a project in a new tab."
  (interactive)
  (other-tab-prefix)
  (projectile-switch-project)
  (tab-rename (projectile-project-name)))

(global-set-key (kbd "C-c w r") 'tab-bar-rename-tab)
(global-set-key (kbd "C-c w k") 'tab-bar-close-tab)
(global-set-key (kbd "C-c w o") 'mono/new-project-tab)

(provide 'mono-projects)

;;; mono-projects.el ends here
