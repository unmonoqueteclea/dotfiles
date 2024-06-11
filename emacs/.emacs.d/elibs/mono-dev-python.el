;;; mono-dev-python.el --- Python development tools  -*- lexical-binding: t -*-

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
;;  Last full review: 2023-05-02
;;
;;; Code:

(require 'mono-base-package)
(require 'mono-dev-tools)
(require 'mono-projects)
(require 'python)

;; remove guess indent python message
(setq python-indent-guess-indent-offset-verbose nil)

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

;; automatically activate pyenv version from Emacs with pyenv-mode.
;; It traverse directories up until .python-version file will be found)
;; and activates pyenv version defined there.
(defun pyenv-mode-auto-hook ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (progn
             (pyenv-mode-set (car (s-lines (s-trim (f-read-text pyenv-version-path 'utf-8)))))
             t))))))

(add-hook 'find-file-hook 'pyenv-mode-auto-hook)

(defun lint-fix-file-and-revert ()
  (interactive)
  (when (derived-mode-p 'python-mode)
    (shell-command (concat "ruff check --fix " (buffer-file-name)))
    ;; equivalent to isort, not done by default --fix
    (shell-command (concat "ruff check --fix --select I " (buffer-file-name)))
    ;; equivalent to black
    (shell-command (concat "ruff format " (buffer-file-name))))
  (revert-buffer t t))

;; force eglot to use pyright always
(require 'eglot)
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))

(add-hook
 'python-mode-hook
 (lambda ()
   (flymake-mode)
   (local-set-key (kbd "C-$") 'flymake-goto-next-error)
   (eglot-ensure)
   (add-hook 'after-save-hook 'lint-fix-file-and-revert nil t)))

;; see https://www.reddit.com/r/emacs/comments/10yzhmn/flymake_just_works_with_ruff/
(setq python-flymake-command '("ruff check" "--quiet" "--stdin-filename=stdin" "-"))
(add-hook 'eglot-managed-mode-hook
   (lambda () (cond
	       ((derived-mode-p 'python-base-mode)
                (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
               (t nil))))


(provide 'mono-dev-python)

;;; mono-dev-python.el ends here
