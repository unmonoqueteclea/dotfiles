;;; mono-help.el --- help utilities -*- lexical-binding: t -*-

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
(require 'mono-buffer) ;; diminish definition

;; which-key is a minor mode for Emacs that displays the key bindings
;; following your currently entered incomplete command (a prefix)
;; in a popup
(which-key-mode)
(setq whcih-key-idle-delay 0.2)


(provide 'mono-help)

;;; mono-help.el ends here
