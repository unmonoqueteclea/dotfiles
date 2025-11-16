;;; mono-base-theme.el --- UI and theme configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2024, 2025  Pablo González Carrizo

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
;;  Some fonts may need to be installed in your system

;;; Code:
(require 'mono-base-package)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)

;; See https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :demand t :config  (modus-themes-load-theme 'ef-autumn))

;; fontaine lets the user specify presets of font configurations and
;; set them on demand on graphical Emacs frames. The user option
;; fontaine-presets holds all such presets.
;; (FONTAINE: FONTs Are Irrelevant in Non-graphical Emacs)
(use-package fontaine
  :straight
  (fontaine :type git :host github :repo "protesilaos/fontaine" :branch "main")
  :defines fontaine-presets
  :demand t
  :config
  (setq fontaine-presets
	'((regular
           :default-family "Hack Nerd Font"
           :default-height 92)
          (large
           :default-family "Hack Nerd Font"
           :default-height 110))))

  ;; recover last preset or fall back to desired style from
  ;; `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

(defun add-borders ()
  "Add frame borders and dividers."
  (modify-all-frames-parameters
   '((right-divider-width . 20)
     (internal-border-width . 20)))
  (dolist (face '(window-divider window-divider-first-pixel window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

(add-borders)
(toggle-frame-fullscreen)

;; when we toggle the theme, we have to add the borders again
;; to ensure the correct color is used
(advice-add 'modus-themes-toggle :after 'add-borders)

;; this package is required by many others to show emojis
(use-package all-the-icons :demand t :if (display-graphic-p))

(provide 'mono-base-theme)

;;; mono-base-theme.el ends here
