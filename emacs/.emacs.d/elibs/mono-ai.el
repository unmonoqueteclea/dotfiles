;;; mono-ai.el --- AI-related packages -*- lexical-binding: t -*-

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
;;  LLM and other AI-related packages.

;; Last full review: 2024-11-17

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-secret)

;; https://github.com/karthink/gptel
(use-package gptel
  :config
  (setq gptel-model :gemini-pro
	gptel-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)))

(use-package gptel-quick
  :config
  (setq gptel-quick-model :gemini-pro
	gptel-quick-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t))
  :straight (:host github :repo "karthink/gptel-quick"))


(defun no-copilot-in-json-mode ()
   (eq major-mode 'json-mode))

;; https://github.com/copilot-emacs/copilot.el
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el")
  :hook (prog-mode . copilot-mode)
  :config
  (add-to-list 'copilot-disable-predicates #'no-copilot-in-json-mode)
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char-warning-disable t)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion))

(use-package aidermacs
  :bind (("C-c A" . aidermacs-transient-menu))
  :config
  (setenv "GEMINI_API_KEY" gemini-api-key)
  :custom
  (aidermacs-use-architect-mode nil)
  (aidermacs-default-model "gemini/gemini-1.5-pro-latest"))

(provide 'mono-ai)

;;; mono-ai.el ends here
