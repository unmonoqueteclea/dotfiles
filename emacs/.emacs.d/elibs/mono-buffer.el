;;; mono-buffer.el --- Buffers configuration -*- lexical-binding: t -*-

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

;; don't ask for confirmation when killing buffer
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; useful to fold JSON, or other kind of files
;; I only use the keybinding <C-M-return>
(use-package yafolding
  :hook (prog-mode . yafolding-mode)
  :config
  (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
  (define-key yafolding-mode-map (kbd "<C-M-return>") 'yafolding-toggle-element)
  (define-key yafolding-mode-map (kbd "<C-return>") nil))

;; this package implements hiding or abbreviation of the mode line
;; displays (lighters) of minor-modes.
;; https://github.com/myrjola/diminish.el
(use-package diminish
  :diminish  pyenv-mode)

;; jinx.el - enchanted spell checker
;; On Debian or Ubuntu, install the packages libenchant-2-dev and pkg-config
(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (setq jinx-languages "en-US es-ES")
  (dolist (hook '(text-mode-hook markdown-mode-hook))
    (add-hook hook #'jinx-mode)))

(use-package doom-modeline
  :init
  (which-function-mode)
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 60
        doom-modeline-buffer-encoding nil
        ;; do not show clocked task in modeline
        org-clock-clocked-in-display nil))


(provide 'mono-buffer)

;;; mono-buffer.el ends here
