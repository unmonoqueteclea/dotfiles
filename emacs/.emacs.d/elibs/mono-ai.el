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

;; Last full review: 2025-05-25

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'mono-dwim)
(require 'mono-secret)

;; See https://github.com/karthink/gptel
;; 2025-05-25: After several issues with OpenRouter integration in gptel (no way to make tool
;; use work, HTTP errors 429, etc) I decided to still using Gemini integration that
;; works very well with gptel.

;; TODO 2025-05-25: Check tools available in https://github.com/skissue/llm-tool-collection/
;; TODO 2025-05-25: Integrate tools from https://github.com/karthink/gptel/wiki/Tools-collection
(use-package gptel
  :config
  (require 'gptel-integrations)
  (setq gptel-model 'gemini-2.0-flash
	gptel-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)))

(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el")
  :config (setq mcp-hub-servers
		`(("brave-search" .
		   (:command "docker"
                    :args ("run" "-i" "--rm" "-e" "BRAVE_API_KEY" "mcp/brave-search")
                    :env (:BRAVE_API_KEY ,secret-brave-api-key)))
		  ("mcp-altassian" .
		   (:command "docker"
		    :args ("run" "-i" "--rm"
			   "-e" "JIRA_URL" "-e" "JIRA_USERNAME" "-e" "JIRA_API_TOKEN"
			   "ghcr.io/sooperset/mcp-atlassian:latest")
                    :env (
			  :JIRA_URL ,secret-jira-base-url
			  :JIRA_USERNAME ,secret-jira-username
			  :JIRA_API_TOKEN ,secret-jira-token
			  ))))))



;; I use very long JSON files and I don't want copilot to interfere with them.
(defun no-copilot-in-json-mode () (eq major-mode 'json-mode))

;; https://github.com/copilot-emacs/copilot.el
;; 2025-05-25: Still no way to chat with copilot or to use a different model
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el")
  :hook (prog-mode . copilot-mode)
  :config
  (add-to-list 'copilot-disable-predicates #'no-copilot-in-json-mode)
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-max-char-warning-disable t)
  (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion))


;; 2025-05-25: TODO: When I have a better integration with gptel, it is very likely
;; that I will be able to remove this.
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
  (let ((command (format "llm-note-ask \"%s\"" prompt)))
    (dwim-shell-command-on-marked-files
     "Ask my notes" command :focus-now t )))

(provide 'mono-ai)

;;; mono-ai.el ends here
