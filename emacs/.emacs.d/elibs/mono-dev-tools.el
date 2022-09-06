;;; mono-dev-tools.el --- software development base tools  -*- lexical-binding: t -*-

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
(require 'mono-tools-terminal)

;; Flycheck: Syntax checking
;; See https://www.flycheck.org/en/latest/
;; TODO Read flycheck guide and improve configuration
(use-package flycheck
  :commands global-flycheck-mode
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-initialize-packages t)
  ;; Emacs doesn't know yet that use-package is a macro, so it goes ahead
  ;; and tries to compile it as a function
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Magit (git interface), nothing to add, probably one of the most
;; important packages in my configuration.
;; See https://github.com/magit/magit
(use-package magit)

;; Emacs integration for Docker!
;; Supports docker containers, images, volumes, networks and docker-compose.
;; See https://github.com/Silex/docker.el
(use-package docker
  :defines docker-container-default-sort-key
  :init
  ;; Order by name so that container from the same
  ;; group appear together
  (setq docker-container-default-sort-key '("Names"))
  (setq docker-compose-command "docker compose")
  :config
  (setq docker-container-shell-file-name shell-file-name))

;; syntax highlight for dockerfiles
(use-package dockerfile-mode)

;; Emacs integration for Kubernetes
(use-package kubel)

(defun mono/docker-ctop (arg)
  "Open ctop tool to monitor a Docker Compose project ARG."
  (interactive
   (list
    (completing-read
     "Choose one: "
     (split-string (shell-command-to-string "docker compose ls --all -q") "\n" t))))
   (mono/exec-in-vterm
    (concat "ctop -i -f " arg " && exit")
    (concat "ctop-" arg)))


;; Client for Language Server Protocol (v3.14). lsp-mode aims to
;; provide IDE-like experience by providing optional integration with
;; the most popular Emacs packages like company, flycheck and
;; projectile.  Additional lsp coonfiguration can be found in
;; language-specfic config files.
;; See https://github.com/emacs-lsp/lsp-mode
;; See https://www.mattduck.com/lsp-python-getting-started.html
(use-package lsp-mode
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :hook
  ((python-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   ;; Auto-format buffer on save
   (lsp-mode .
     (lambda ()
      (add-hook 'before-save-hook #'lsp-format-buffer t)
      (add-hook 'before-save-hook #'lsp-organize-imports t))))
  :config
  (setq lsp-idle-delay 0.3
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil))

;; This package contains all the higher level UI modules of lsp-mode
;; By default, lsp-mode automatically activates lsp-ui unless
;; lsp-auto-configure is set to nil.\
;; See https://github.com/emacs-lsp/lsp-iu
(use-package lsp-ui :commands lsp-ui-mode
  :commands lsp-ui-mode
  :config
  ;; Show informations of the symbols on the current line. It also
  ;; show flycheck diagnostics and LSP code actions
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-update-mode t)
  ;; Add peek feature
  ;; You may remap xref-find-{definitions,references}
  ;; (bound to M-. M-? by default):
  (lsp-ui-peek-enable t)
  (define-key lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map
    [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; Show object documentation at point in a child frame.
  (lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame))

;; Syntax highlighting for YAML files
(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(global-set-key (kbd "C-c w c") 'mono/docker-ctop)

(provide 'mono-dev-tools)

;;; mono-dev-tools.el ends here
