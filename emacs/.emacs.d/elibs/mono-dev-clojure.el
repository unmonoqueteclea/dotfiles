;;; mono-dev-clojure.el --- Clojure development tools  -*- lexical-binding: t -*-

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
;;  Last full review: 2023-09-29
;;
;;; Code:

(require 'mono-base-package)

(use-package cider
  :config
  (defun clerk-show ()
    (interactive)
    (when-let
	((filename
          (buffer-file-name)))
      (save-buffer)
      (message (concat "(nextjournal.clerk/show! \"" filename "\")"))
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))
  (define-key clojure-mode-map (kbd "<M-return>") 'clerk-show))

(defun add-clj-format-on-save-hook ()
  (add-hook 'before-save-hook 'cider-format-buffer t t))
(add-hook 'clojure-mode-hook 'add-clj-format-on-save-hook)



(provide 'mono-dev-clojure)

;;; mono-dev-clojure.el ends here
