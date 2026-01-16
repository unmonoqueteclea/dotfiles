;;; mono-buffer.el --- Buffers configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022, 2024, 2026  Pablo González Carrizo

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
;;  Last full review: 2026-01-16

;;; Code:
(require 'mono-base-package)

(setq-default buffer-file-coding-system 'utf-8-unix)

;; Don't ask for confirmation when killing buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Useful to fold JSON, or other kind of files
(use-package yafolding
  :hook (prog-mode . yafolding-mode)
  :config
  ;; I only need the keybinding <C-M-return>
  (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-M-return>") 'yafolding-toggle-element)
  (define-key yafolding-mode-map (kbd "<C-return>") nil))

;; This package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
(use-package diminish)

;; Font nerd-icons is necessary. Then run M-x nerd-icons-install-fonts to
;; install the resource fonts.
(use-package doom-modeline
  :init
  (which-function-mode)
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30
        doom-modeline-buffer-encoding nil
        ;; do not show clocked task in modeline
        org-clock-clocked-in-display nil))


;; Default max line size for different modes
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1) (setq fill-column 100)))
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 1) (setq fill-column 90)))
(add-hook 'prog-mode-hook (lambda () (setq fill-column 100)))

;; Window splitting behavior:
;; - Allow vertical splits only when the window is very wide (>= 200 columns).
;; - Always allow horizontal splits.
;; Result: Emacs splits vertically only on ultrawide frames; otherwise it defaults to horizontal.
(setq split-width-threshold 200)
(setq split-height-threshold nil)
(setq split-width-threshold 200)
(setq split-height-threshold nil)


(provide 'mono-buffer)

;;; mono-buffer.el ends here
