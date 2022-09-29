;;; mono-blog.el --- blogging from emacs -*- lexical-binding: t -*-

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
;;  I use org-static-blog for blogging.

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'f)

(use-package org-static-blog)
(setq org-static-blog-publish-title "@unmonoquetecles")
(setq org-static-blog-publish-url "https://unmonoqueteclea.github.io")
(setq org-static-blog-publish-directory (concat mono-dir-vc "/unmonoqueteclea.github.io"))
(setq org-static-blog-posts-directory (concat org-static-blog-publish-directory "/posts"))
(setq org-static-blog-drafts-directory (concat org-static-blog-publish-directory "/drafts"))
(setq org-static-blog-enable-tags t)
(setq org-static-blog-enable-tag-rss t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
;; this header is inserted into the <head> section of every page:
(setq org-static-blog-page-header
       (f-read-text (concat org-static-blog-publish-directory "/public/header.html") 'utf-8))
;; this preamble is inserted at the beginning of the <body> of every page:
(setq org-static-blog-page-preamble
      (f-read-text (concat org-static-blog-publish-directory "/public/preamble.html") 'utf-8))
;; this postamble is inserted at the end of the <body> of every page:
(setq org-static-blog-page-postamble
      (f-read-text (concat org-static-blog-publish-directory "/public/postamble.html") 'utf-8))
;; this HTML code is inserted into the index page between the
;; preamble and the blog posts
(setq org-static-blog-index-front-matter
      (f-read-text (concat org-static-blog-publish-directory "/public/title.html") 'utf-8))
;; number of articles on the index page
(setq org-static-blog-index-length 5)
;; by default, show preview of posts
(setq org-static-blog-use-preview t)

(provide 'mono-blog)

;;; mono-blog.el ends here
