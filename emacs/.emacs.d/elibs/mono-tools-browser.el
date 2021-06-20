;;; mono-tools-browser.el --- use Emacs as a web browser -*- lexical-binding: t -*-

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

(setq browse-url-firefox-program "/usr/bin/firefox")
(setq browse-url-generic-program browse-url-firefox-program)
(setq browse-url-browser-function 'browse-url-generic)

(provide 'mono-tools-browser)

;;; mono-tools-browser.el ends here
