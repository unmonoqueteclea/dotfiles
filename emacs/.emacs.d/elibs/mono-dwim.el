;;; mono-dwim.el --- dwim shell commands -*- lexical-binding: t -*-

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
;;  This package contains commands to use with `dwim-shell-package`.
;;  Use dwim-shell-command-on-marked-files to easily integrate
;;  command-line utilities into frequent Emacs workflows
;;  See https://github.com/xenodium/dwim-shell-command

;;; Code:

(require 'mono-base-package)


(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
   :map dired-mode-map
   ([remap dired-do-async-shell-command] . dwim-shell-command)
   ([remap dired-do-shell-command] . dwim-shell-command)
   ([remap dired-smart-shell-command] . dwim-shell-command)))

;; load some default commands
(require 'dwim-shell-commands)

(defun mono/dwim-suspend ()
  "Just suspend system."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Suspending system"
   "systemctl suspend"
   :utils "systemctl"
   :no-progress t
   :error-autofocus t
   :silent-success t))

(defun mono/dwim-validate-json ()
  "Validate JSON file using jsonlint"
  (interactive)
  (dwim-shell-command-on-marked-files
   "Validate JSON file"
   "jsonlint <<f>>"
   :error-autofocus t
   :silent-success t))

(provide 'mono-dwim)
;;; mono-dwim.el ends here
