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
  (eval-after-load 'web-mode
    '(add-hook 'vue-mode-hook 'add-node-modules-path)))

;; configure flycheck for web-mode
(with-eval-after-load 'flycheck
  ;; use eslint_d if you want a faster linter
  ;;(setq flycheck-javascript-eslint-executable "eslint_d")
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))


;; automatically execute eslint-fix on save
(use-package eslint-fix
  :demand t
  :config
  (eval-after-load 'vue-mode
    '(add-hook 'vue-mode-hook
	       (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
	       (lambda () (add-hook 'after-save-hook 'eslint-fix nil t)))))


;; run prettier on save
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.
MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
          (funcall (cdr my-pair)))))

(use-package prettier-js
  :demand t
  :config
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode
                                '("\\.js\\'" . prettier-js-mode)))))

(provide 'mono-dev-web)
;;; mono-dev-web.el ends here
