;;; mono-tools-terminal.el --- shells and terminals -*- lexical-binding: t -*-

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

;; using bash as default shell
(setq shell-file-name "/bin/bash")
;; force shell to be interactive so that we have access to all
;; functions and aliases
(setq shell-command-switch "-ic")

(use-package vterm
  :defines
  vterm-kill-buffer-on-exit
  vterm-copy-exclude-prompt
  vterm-use-vterm-prompt-detection-method
  vterm-buffer-name-string
  :init
  (setq vterm-kill-buffer-on-exit t
        vterm-copy-exclude-prompt t
        vterm-use-vterm-prompt-detection-method t
        vterm-buffer-name-string nil
	vterm-shell shell-file-name))

(require 'vterm)

(defun mono/named-vterm (term-name)
  "Generate a vterm terminal with buffer name TERM-NAME."
  (vterm (concat "term-" term-name)))

(defun mono/exec-in-vterm (cmd buff)
  "Execute a command CMD in vterm in a buffer called BUFF."
  (switch-to-buffer (generate-new-buffer buff))
  (vterm-mode)
  (vterm--goto-line -1)
  (vterm-send-string cmd)
  (vterm-send-return))

(defun mono/vterm (term-name)
  "Create a new space with a vterm terminal with buffer name TERM-NAME."
  (interactive "sTerminal buffer name: ")
  (other-tab-prefix)
  (mono/named-vterm term-name))

(provide 'mono-tools-terminal)

;;; mono-tools-terminal.el ends here
