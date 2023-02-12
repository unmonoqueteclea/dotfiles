;;; mono-tools-email.el --- read email from Emacs -*- lexical-binding: t -*-

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

;; Use imagemagick if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; mu4e is part of mu — by installing the latter, the former is installed as well.
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mu4e-maildir "~/Mail")

;; check .mbsyncrc file
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-view-show-images t)
(setq mu4e-view-prefer-html t)
(setq mu4e-view-use-gnus t)

(provide 'mono-tools-email)

;;; mono-tools-email.el ends here
