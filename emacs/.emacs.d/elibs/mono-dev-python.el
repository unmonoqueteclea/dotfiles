;;; mono-dev-python.el --- Python development tools  -*- lexical-binding: t -*-

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
;;
;;; Code:

(require 'mono-base-package)
(require 'mono-dev-tools)
(require 'mono-projects)
(require 'python)

;; remove guess indent python message
(setq python-indent-guess-indent-offset-verbose nil)

;; configure ipython as my default python shell
(setq python-shell-completion-native-disabled-interpreters '("ipython"))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--profile=emacs -i --simple-prompt")

;; integrate pytest within emacs
;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest :commands python-pytest-dispatch)

;; to manage python versions and virtualenvs I am using pyenv. It lets
;; you easily switch between multiple versions of Python. It's simple,
;; unobtrusive, and follows the UNIX tradition of single-purpose tools
;; that do one thing well.

;; pyenv mode integrates Fabián E. Gallina's python.el with the pyenv
;; tool. This gives packages which already use python.el (like
;; python-django) pyenv virtual environment support out-of-the-box.
;; Pyenv setup the PYENV_VERSION environment variable and
;; python-shell-virtualenv-path custom variable based on user input
(use-package pyenv-mode :demand t :commands pyenv-mode :config  (pyenv-mode))
;; pyenv tries to override C-c C-s keybind that I am using in other places
(eval-after-load "pyenv-mode" (define-key pyenv-mode-map (kbd "C-c C-s") nil))

;; some additional functions that complement pyenv-mode
(defun mono/pyenv-versions ()
  "Show the list of pyenv versions."
  (interactive)
  (shell-command "pyenv versions"))

(defun mono/pyenv-create-env (parent name)
  "Create pyenv version from PARENT Python version and evn NAME."
  (interactive "sPython version to use: \nsEnvironment name: ")
  (shell-command (concat "pyenv virtualenv " parent " " name)))

(defun mono/pyenv-remove-env (name)
  "Remove pyenv version from its NAME."
  (interactive
   (list
    (completing-read "Choose one: " (pyenv-mode-versions))))
  (shell-command (concat "pyenv virtualenv-delete -f " name)))

;; with this, we ensure that we are always using the right environment
;; associated to a specific projectile project.
;; copied from pyenv.el doc
(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))
(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

;; the same as before, but for non-projectile projects
;; automatically activate pyenv version from Emacs with pyenv-mode.
;; It traverse directories up until .python-version file will be found
;; and activates pyenv version defined there.
(use-package pyenv-mode-auto :demand t)

;; TODO when I upgrade to emacs 29 I won't need this, as it is already
;; included by default
(use-package eglot
  :config
  ;; shutdown server after killing last managed buffer
  (setq eglot-autoshutdown t))

(add-hook
 'python-mode-hook
 (lambda ()
   (eglot-ensure)
   ;; ensure eglot works with python and formats on save
   (add-hook 'before-save-hook 'eglot-format nil t)))


;; recommendation from lsp-mode docs
;; not sure if it is already useful for eglot
(setq read-process-output-max (* 1024 1024))

(provide 'mono-dev-python)

;;; mono-dev-python.el ends here
