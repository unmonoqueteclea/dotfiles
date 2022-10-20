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

;; use pyenv to manage Python versions within Emacs
;; Pyenv mode integrates Fabián E. Gallina's python.el with the pyenv
;; tool.  This gives packages which already use python.el
;; (like python-django) pyenv virtual environment support out-of-the-box.
;; Pyenv setup the PYENV_VERSION environment variable and
;; python-shell-virtualenv-path custom variable based on user input
(use-package pyenv-mode :commands pyenv-mode :config  (pyenv-mode))
;; Automatically activate pyenv version from Emacs with pyenv-mode.
;; It traverse directories up until .python-version file will be found
;; and activates pyenv version defined there.
(use-package pyenv-mode-auto)


;; configuration of lsp-mode for python
;; See https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; See https://www.mattduck.com/lsp-python-getting-started.html
;; In your project, you should do:
;;   - pip install python-language-server[all]
;;   - pip install pyls-black pyls-isort pyls-mypy
;;
(require 'lsp-mode)
(require 'lsp-pylsp)
(setq lsp-pylsp-plugins-autopep8-enabled nil ;; we are using black instead
      lsp-pylsp-plugins-flake8-enabled t
      lsp-pylsp-plugins-jedi-completion-enabled t
      lsp-pylsp-plugins-jedi-use-pyenv-environment t
      lsp-pyls-plugins-rope-completion-enabled nil  ;; we are not using rope
      lsp-pylsp-plugins-pycodestyle-enabled nil  ;; we are not using pycodestyle
      lsp-pylsp-plugins-pydocstyle-enabled nil
      lsp-pylsp-plugins-pylint-enabled nil ;; we are using flaky instead pylint
      lsp-pylsp-plugins-yapf-enabled nil) ;; we are not usng yapf

(lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))


(use-package python-pytest
  :commands python-pytest-dispatch)

(setq python-shell-completion-native-disabled-interpreters '("ipython"))
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--profile=emacs -i --simple-prompt")

(provide 'mono-dev-python)

;;; mono-dev-python.el ends here
