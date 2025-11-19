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
;;  Last full review: 2024-07-10
;;
;;; Code:

(require 'mono-base-package)
(require 'mono-dev-tools)
(require 'mono-projects)
(require 'python)

;; followed that guide to enable tree-sitter (and also using treesit-auto)
;; https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7

;; remove guess indent python message
(setq python-indent-guess-indent-offset-verbose nil)

;; integrate pytest within emacs
;; https://github.com/wbolster/emacs-python-pytest
(use-package python-pytest :commands python-pytest-dispatch)

(setq doom-modeline-env-enable-python nil)

(use-package pyvenv :commands (python-activate python-workon))

(defun mono/auto-activate-venv ()
  "Automatically activate virtualenv if .venv exists in project root."
  (let* ((project-root (or (locate-dominating-file default-directory ".venv") default-directory))
	 (venv-path (expand-file-name ".venv" project-root)))
    (when (file-directory-p venv-path)
      (message "Activating virtualenv: %s" venv-path)
      (pyvenv-activate venv-path))))


;; force eglot to use pyright always
;; pyright offers a langserver, so that it is fully integrated
;; sadly, eglot doesn't support multiple servers so we cannot use
;; ruff-lsp (or similar ones)
(require 'eglot)
(add-to-list 'eglot-server-programs
	     '((python-mode python-ts-mode)
	       "basedpyright-langserver" "--stdio"))

(defun lint-fix-file-and-revert ()
  (interactive)
  (shell-command (concat "uv run ruff format " (buffer-file-name)))
  (shell-command (concat "uv run ruff check --fix --extend-select I " (buffer-file-name)))
  (revert-buffer t t t))

(defun mono/python-save-checks ()
  (mono/auto-activate-venv)
  (eglot-ensure)
  (flymake-mode)
  (local-set-key (kbd "C-$") 'flymake-goto-next-error)
  (eldoc-box-hover-at-point-mode)
  (add-hook 'after-save-hook 'lint-fix-file-and-revert nil t))

(add-hook 'python-mode-hook (lambda () (mono/python-save-checks)))


;; IMPORTANT (2024-07-10) After several tests, I wasn't able to make Flymake
;; work with pyright (through eglot) and ruff at the same time...
;; I tested many thing (below) but none of them worked, it seems like
;; eglot hijacks flymake to only show its messages.

;; see https://www.reddit.com/r/emacs/comments/10yzhmn/flymake_just_works_with_ruff/
;;(add-hook 'python-base-mode-hook 'flymake-mode)
;;(setq python-flymake-command '("ruff" "check"  "--quiet" "--stdin-filename=stdin" "-"))

;; (use-package flymake-ruff)
;; (add-hook 'python-mode-hook #'flymake-ruff-load)

;;(setq eglot-stay-out-of '(flymake))
;; (add-hook 'eglot-managed-mode-hook
;;           (lambda () (when (derived-mode-p 'python-base-mode)
;;                        ;;(add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
;;                        (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))))


(use-package eldoc-box :demand t)

(provide 'mono-dev-python)

;;; mono-dev-python.el ends here
