;;; mono-docs.el --- handle documents  -*- lexical-binding: t -*-

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
;;

;;; Code:

(require 'mono-base-package)

;; PDF Tools is, among other things, a replacement of DocView for PDF
;; files. The key difference is that pages are not pre-rendered by
;; e.g. ghostscript and stored in the file-system, but rather created
;; on-demand and stored in memory.
;; See https://github.com/politza/pdf-tools
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (define-key pdf-view-mode-map (kbd "C-o") 'pdf-occur)
  (pdf-tools-install))

;; syntax highlight and edit features for markdown files
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode :demand t)


;; translate using several engines
;; https://github.com/lorniu/go-translate
(use-package go-translate
  :config
  (setq gts-translate-list '(("en" "es") ("es" "en")))
  (setq gts-default-translator
	(gts-translator
	 :picker (gts-prompt-picker)
	 :engines (list (gts-bing-engine) (gts-google-engine))
	 :render (gts-buffer-render))))

(provide 'mono-docs)

;;; mono-docs.el ends here
