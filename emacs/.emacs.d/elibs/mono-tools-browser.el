;;; mono-tools-browser.el --- use Emacs as a web browser -*- lexical-binding: t -*-

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


(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
  Default to the URL around or before point.  The strings in
  variable `browse-url-firefox-arguments' are also passed to
  Firefox.

  When called interactively, if variable
  `browse-url-new-window-flag' is non-nil, load the document in a
  new Firefox window, otherwise use a random existing one.  A
  non-nil interactive prefix argument reverses the effect of
  `browse-url-new-window-flag'.

  If `browse-url-firefox-new-window-is-tab' is non-nil, then
  whenever a document would otherwise be loaded in a new window, it
  is loaded in a new tab in an existing window instead.

  When called non-interactively, optional second argument
  NEW-WINDOW is used instead of `browse-url-new-window-flag'."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment))
           (window-args (if (browse-url-maybe-new-window new-window)
                            (if browse-url-firefox-new-window-is-tab
                                '("-new-tab")
                              '("-new-window"))))
           (ff-args (append browse-url-firefox-arguments window-args (list url)))
           (process-name (concat "firefox " url))
           (process (apply 'start-process process-name nil
                           browse-url-firefox-program ff-args) ))))

(setq browse-url-browser-function 'browse-url-firefox)
(provide 'mono-tools-browser)

;;; mono-tools-browser.el ends here
