;;; mono-tools-feeds.el --- reading RSS feeds -*- lexical-binding: t -*-

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
(require 'mono-base-definitions)

;; open news directly in the browser
(defun mono/elfeed-firefox-open (&optional _use-generic-p)
  "From search view, open with Firefox."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url-firefox it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


;; to read hackernews from emacs, I use it from elfeed. It generates
;; an org buffer with the content and the comments of a hacker news
;; post i need to use my own fork to ensure that the link of the post
;; uses org-syntax (so that I can store it in pocket using
;; pocket-add-link) IMPORTANT: Instead of using the official HN RSS
;; (that links to articles) we are using https://hnrss.org/frontpage
;; that links to comments page
(use-package hnreader
  :straight (hnreader :type git :host github :repo "unmonoqueteclea/emacs-hnreader"))

;; to read reddit from emacs, I use this from elfeed
(use-package reddigg)

;; connect emacs to pocket, allowing things like storing links
(use-package pocket-reader)

;; original from https://blog.dornea.nu/2023/04/21/read-hackernews-and-reddit-the-emacs-way/
(defun mono/elfeed-reddit-show-commments (&optional link)
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (if link link (elfeed-entry-link entry))))
    (reddigg-view-comments link)))

;; original from https://blog.dornea.nu/2023/04/21/read-hackernews-and-reddit-the-emacs-way/
(defun mono/elfeed-hn-show-commments (&optional link)
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (if link link (elfeed-entry-link entry))))
    (setq-local hnreader-view-comments-in-same-window nil)
    (hnreader-comment (format "%s" link))))

;; elfeed is an extensible web feed reader for Emacs, supporting both
;; Atom and RSS.
(use-package elfeed
  :commands elfeed
  :functions
  elfeed-search-selected
  elfeed-untag
  elfeed-entry-link
  elfeed-search-update-entry
  :defines
  elfeed-db-directory
  elfeed-curl-max-connections
  elfeed-search-filter
  elfeed-search-mode-map
  :init
  (setq elfeed-db-directory (expand-file-name "elfeed" mono-dir-cache))
  :config
  (setq elfeed-curl-max-connections 2
        elfeed-search-filter "@1-week-ago +unread +fav")
  (define-key elfeed-search-mode-map (kbd "w") 'mono/elfeed-firefox-open)
  (define-key elfeed-search-mode-map (kbd "r") 'mono/elfeed-reddit-show-commments)
  (define-key elfeed-search-mode-map (kbd "h") 'mono/elfeed-hn-show-commments))

;; store feeds in an org file
(use-package elfeed-org
  :commands elfeed-org
  :defines
  rmh-elfeed-org-files
  :functions elfeed-org
  :init
  (setq rmh-elfeed-org-files
        (list (expand-file-name "feeds.org" mono-dir-emacs)))
  (elfeed-org))

;; better Elfeed UI
(use-package elfeed-goodies
  :demand t
  :functions elfeed-goodies/setup
  :config (elfeed-goodies/setup))

(provide 'mono-tools-feeds)

;;; mono-tools-feeds.el ends here
