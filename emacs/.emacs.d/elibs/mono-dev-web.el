;;; mono-dev-web.el --- utilities for web development -*- lexical-binding: t -*-

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
(require 'mono-dwim)

;; web-mode is the main mode to work with HTML, CSS or Javascript code.
;; It provides smart indentation, jumping with C-c C-n between
;; open/closing HTML tags, dom navigation, code folding with C-c
;; C-f, etc
(use-package web-mode
  :demand t
  :custom
  (web-mode-markup-indent-offset 2) ;; HTML element offset indentation
  (web-mode-code-indent-offset 2) ;; Script/code offset indentation
  (web-mode-css-indent-offset 2)  ;; CSS offset indentation
  (web-mode-attr-indent-offset t) ;; Tag attributes indented
  (web-mode-style-padding 0) ;; Left padding for <style> parts
  (web-mode-script-padding 0) ;; Left padding for <script> parts
  (web-mode-block-padding 0) ;; Left padding for multiline blocks
  (web-mode-enable-auto-pairing t) ;; Auto Pairing
  (web-mode-enable-css-colorization t );; CSS Colorization
  ;; Highlight current element and column
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.css\\'" . web-mode)
   ("\\.less\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.hbs\\'" . web-mode))
  :config
  (require 'web-mode)
  :bind
  (:map
   web-mode-map
   ("C-M-f" . web-mode-forward-sexp)
   ("C-M-b" . web-mode-backward-sexp))
  :hook
  (web-mode . (lambda () (whitespace-mode -1))))

;; define a derived mode from web-mode that we will
;; use for .vue files.
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; this package provides add-node-modules-path, which searches the
;; current files parent directories for the node_modules/.bin/
;; directory and adds it to the buffer local exec-path. This allows
;; Emacs to find project based installs of e.g. eslint.
(use-package add-node-modules-path
  :demand t
  :config
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook 'add-node-modules-path))
  (eval-after-load 'vue-mode
    '(add-hook 'vue-mode-hook 'add-node-modules-path)))

;; automatically execute eslint-fix on save
(use-package eslint-fix
  :demand t
  :config
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
	       (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
  (eval-after-load 'vue-mode
    '(add-hook 'vue-mode-hook
	       (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))

;; we could run prettier (with prettier-js) but, for now, eslint (with eslint-fix) is enough

;; https://github.com/leafOfTree/svelte-mode
(use-package svelte-mode)

;; see https://ag91.github.io/blog/2022/11/15/catch-you-js-consolelog-you-forgot-to-remove-with-emacs-and-magit/
(defun ag91/find-console-log-in-js-staged-files ()
  "Warn if there are console.log in staged files."
  (when  (s-contains-p "\.js" (s-join " " (magit-staged-files)))
    (--> (shell-command-to-string "git diff --cached")
         s-lines
         (--keep (when (and (s-starts-with-p "+" it) (s-contains-p "console\." it))
                   (substring it 1 (length it)))
                 it)
         (--each it
           (warn (buttonize
		  (format "You have a console.* in commit: %s" it)
		  `(lambda (x)
                     (let ((default-directory ,default-directory))
                       (magit-diff-staged))
                     (goto-char (point-min))
                     (search-forward ,it nil t))))))))

;; before saving a json file, pretty print it
;; after saving it, validate it with a command line tool
(use-package json-mode)
(add-hook 'json-mode-hook
	  (lambda()
	    ;; final nil t ensure before-save-hook is buffer local
	    (add-hook 'before-save-hook 'json-pretty-print-buffer nil t)
	    (add-hook 'after-save-hook 'mono/dwim-validate-json nil t)))

(add-hook 'magit-post-stage-hook 'ag91/find-console-log-in-js-staged-files)


(provide 'mono-dev-web)
;;; mono-dev-web.el ends here
