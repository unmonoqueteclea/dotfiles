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
;;  Last full review: 2022-12-28


;;; Code:

(require 'mono-base-package)
(require 'mono-tools-terminal)

;; automatically remove trailing whitespace when saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; magit (git interface), nothing to add, probably one of the most
;; important packages in my configuration.
;; https://github.com/magit/magit
(use-package magit)

;; show TODOs in magit UI
(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;; pull requests and issues from magit interface
;; (needs .authinfo file with github/gitlab token)
(use-package forge :after magit)

;; walk through git revisions of a file
;; https://github.com/emacsmirror/git-timemachine
(use-package git-timemachine)

;; git-gutter.el is an Emacs port of the Sublime Text plugin GitGutter
;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :demand t
  :diminish git-gutter-mode
  :config
  (setq git-gutter:added-sign "+")
  (setq git-gutter:modified-sign "*")
  (setq git-gutter:hide-gutter t)
  (global-git-gutter-mode +1))

;; interactive Emacs functions that create URLs for files and commits
;; in repositories https://github.com/sshaw/git-link
;; If git-timemachine-mode is active git-link generates a URL for the
;; version of the file being visited.
(use-package git-link)

;; emacs integration for Docker!
;; supports docker containers, images, volumes, networks and docker-compose.
;; see https://github.com/Silex/docker.el
(use-package docker
  :defines docker-container-default-sort-key
  :init
  ;; Order by name so that container from the same
  ;; group appear together
  (setq docker-container-default-sort-key '("Names"))
  (setq docker-compose-command "docker compose")
  :config
  (setq docker-container-shell-file-name shell-file-name))

;; emacs integration for Kubernetes
;; https://github.com/abrochard/kubel
(use-package kubel)

;; syntax highlight for dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode)

;; public function to show ctop
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
(global-set-key (kbd "C-c w c") 'mono/docker-ctop)

;; syntax highlighting for YAML files
;; see https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :init (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; CIDER is the Clojure(Script) Interactive Development Environment
;; that Rocks!
;; https://docs.cider.mx/cider/index.html
(use-package cider)

;; syntax highlight and some other functions for haskell
;; see https://github.com/haskell/haskell-mode
(use-package haskell-mode)

;; syntax highlight and utilities for BigML's WhizzML mode
(use-package whizzml-mode)

;; the only cheatsheet you need
;; https://github.com/chubin/cheat.sh
(use-package cheat-sh
  :config (global-set-key (kbd "C-h C-h") 'cheat-sh))


(provide 'mono-dev-tools)

;;; mono-dev-tools.el ends here
