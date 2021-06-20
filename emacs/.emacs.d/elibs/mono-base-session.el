;;; mono-base-session.el --- Configuration for sessions -*- lexical-binding: t -*-

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
;;  Configurations for sessions, backups, history, etc

;;; Code:
(require 'mono-base-definitions)
(require 'savehist)
(require 'recentf)
(require 'bookmark)

(setq inhibit-startup-screen t)
;; ask before killing emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; save miscellaneous history
(setq savehist-additional-variables
      '(kill-ring
        command-history
	set-variable-value-history
	custom-variable-history
	query-replace-history
	read-expression-history
	minibuffer-history
	read-char-history
	face-name-history
	bookmark-history
        ivy-history
	counsel-M-x-history
	file-name-history
        counsel-minibuffer-history))

(setq history-length 250)
(setq kill-ring-max 20)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)

;; persist history over Emacs restarts.
;; completion frameworks sometimes order items using history
(setq savehist-file (expand-file-name "history" mono-dir-cache))
(savehist-mode 1)

;; remove text properties for kill ring entries
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  "Remove properties from elements of kill ring."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'savehist-save-hook 'unpropertize-kill-ring)

;; recentf files
(setq recentf-max-menu-items 40)
(setq recentf-save-file (expand-file-name "recentf" mono-dir-cache))
(recentf-mode 1)

;; bookmarks are stored in cache
;; TODO Think if I should store bookmarks in a persistent place
(setq bookmark-default-file (expand-file-name "bookmarks" mono-dir-cache))

;; backup files configuration
(setq backup-directory-alist
      `(( "." . ,(expand-file-name "backup" mono-dir-cache))))
(setq make-backup-files t     ; backup of a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 3     ; oldest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      kept-new-versions 3     ; newest versions to keep when a new numbered
                              ;  backup is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
                              ;  (default: 30)
      auto-save-interval 200) ; number of keystrokes between auto-saves
                              ;  (default: 300)

(provide 'mono-base-session)

;;; mono-base-session.el ends here
