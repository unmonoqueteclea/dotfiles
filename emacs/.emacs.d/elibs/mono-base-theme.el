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


;; See https://github.com/protesilaos/modus-themes
;; The overarching objective of this project is to always offer accessible
;; color combinations. Currently not used, because I prefer some themes
;; from ef-themes
;; (use-package emacs
;;   :defines modus-themes-org-agenda
;;   :init
;;   ;; add all your customizations prior to loading the themes
;;   ;; See mono-agenda for the rest of org-agenda configuration
;;   (setq modus-themes-org-agenda
;; 	'((header-block . (variable-pitch semibold 1.1))
;;           (header-date . (underline-today 1.1))
;;           (event . (accented varied))
;;           (scheduled . rainbow)
;;           (habit . traffic-light)))
;;   :config
;;   ;; load the theme of your choice: modus-operandi or modus-vivendi
;;   (load-theme 'modus-operandi))

;; See https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :demand t :config (ef-themes-select 'ef-light))

;; fontaine lets the user specify presets of font configurations and
;; set them on demand on graphical Emacs frames. The user option
;; fontaine-presets holds all such presets.
;; (FONTAINE: FONTs Are Irrelevant in Non-graphical Emacs)

;; Presets consist of a list of properties that govern the family,
;; weight, and height of the faces default, fixed-pitch,
;; variable-pitch, bold, and italic. Each preset is identified by a
;; user-defined symbol as the car of a property list.
;; IMPORTANT Iovseka font can be installed from
;; https://git.sr.ht/~protesilaos/iosevka-comfy
(use-package fontaine
  :straight
  (fontaine :type git :host github :repo "protesilaos/fontaine" :branch "main")
  :defines fontaine-presets
  :demand t
  :config
  (setq fontaine-presets
	`((regular :default-height 93)
          (large :default-height 103)
          (presentation :default-height 160)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Monospace"
           :default-weight regular
           :default-height 93
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil))))

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
