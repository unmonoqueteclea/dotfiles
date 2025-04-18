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
(require 'mono-dwim)
(require 'mono-secret)

(setq gemini-pro "openrouter/google/gemini-2.5-pro-exp-03-25:free")
(setq gemini-flash "gemini/gemini-2.0-flash")

;; https://github.com/karthink/gptel
(use-package gptel
  :config
  (setq gptel-model :gemini-pro
	gptel-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)))

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
  :bind (("C-c s" . aidermacs-transient-menu))
  :config
  (setenv "GEMINI_API_KEY" gemini-api-key)
  (setenv "OPENROUTER_API_KEY" openrouter-api-key)
  :custom
  (aidermacs-auto-commits nil)
  (aidermacs-show-diff-after-change t)
  (aidermacs-backend 'vterm)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model gemini-pro)
  (aidermacs-architect-model gemini-pro)
  (aidermacs-editor-model gemini-pro))

(defun mono/llm-commit-message ()
  "Generate a commit-message using LLM and copy it to clipboard."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Generate git commit message"
   "llm-commit-msg"
   :error-autofocus t))

(defun mono/llm-note-ask (prompt)
  "Execute llm-note-ask with PROMPT and display the result."
  (interactive "sPrompt: ")
  (let ((command (format "llm-notes-ask \"%s\"" prompt)))
    (dwim-shell-command-on-marked-files
     "Ask my notes" command :focus-now t )))

(defun mono/llm-meeting-ask (prompt)
  "Execute llm-meeting-ask with PROMPT and display the result."
  (interactive "sPrompt: ")
  (let ((command (format "llm-meeting-ask \"%s\"" prompt)))
    (dwim-shell-command-on-marked-files
     "Ask my meetings" command :focus-now t )))

(defun mono/llm-replace-region (start end)
  "Send selected text to shell command with a default prompt to fix errors and replace it with the output."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (default-prompt "Rewrite this text, fixing any kind of error, and return only the rephrased version.")
         (prompt (read-string "Enter LLM prompt: " default-prompt))
         (command (format "echo %s | llm -s \"%s\"" (shell-quote-argument text) (shell-quote-argument prompt)))
         (output (string-trim (shell-command-to-string command))))
    (delete-region start end)
    (insert output)))


(provide 'mono-ai)

;;; mono-ai.el ends here
