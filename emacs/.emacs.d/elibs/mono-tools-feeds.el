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

;; connect emacs to pocket, allowing things like storing links
(use-package pocket-reader)
(global-set-key (kbd "C-c n p") 'pocket-reader-generic-add-link)

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
  (define-key elfeed-search-mode-map (kbd "w") 'mono/elfeed-firefox-open))

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
