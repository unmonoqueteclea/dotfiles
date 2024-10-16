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

;;  I handle static files (css files, images, or any other kind of
;;  content) in an unusual way.  I place them in
;;  'blog-static-directory', but they are moved automatically every
;;  time we execute 'org-static-blog-publish' to the right location
;;  where they will be consumed: 'blog-publish-static-directory'
;;  (inside 'org-static-blog-publish-directory').  In that way, we can
;;  always safely remove the 'org-static-blog-publish-directory'
;;  folder, and re-generate all the blog files.

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)
(require 'f)

(use-package org-static-blog)
(defconst blog-root-directory
  (concat mono-dir-vc "/unmonoqueteclea.github.io")
  "The path of the directory that contains all blog files.")

(setq org-static-blog-publish-title "@unmonoqueteclea")
(setq org-static-blog-publish-url "https://unmonoqueteclea.github.io")
(setq org-static-blog-rss-file "feed.xml")
;; we move all the auto-generated blog files to 'docs' folder so that,
;; to remove all of them, we can safely remove that folder.  we are
;; using the /docs folder because it is the one deployed by Github
;; docs
(setq org-static-blog-publish-directory (concat blog-root-directory "/docs"))
(defconst blog-static-directory
  (concat blog-root-directory "/static")
  "This directory contains all (non posts) statics files needed in the blog.")
(defconst blog-publish-static-directory
  (concat org-static-blog-publish-directory "/static")
  "The directory where all the files in `blog-static-directory` should be moved.")

(setq org-static-blog-posts-directory (concat blog-root-directory "/posts"))
(setq org-static-blog-drafts-directory (concat blog-root-directory "/drafts"))
(setq org-static-blog-enable-tags t)
(setq org-static-blog-enable-tag-rss t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
;; number of articles on the index page
(setq org-static-blog-index-length 5)
;; by default, show preview of posts
(setq org-static-blog-use-preview t)
;; ellipsis configuration
(setq org-static-blog-preview-link-p t) ;; make it clickable
(setq org-static-blog-preview-ellipsis "Read more...")
(setq org-static-blog-enable-og-tags t)
(setq org-static-blog-image "static/profile.png")

(defun mono/blog-update-statics (&rest _)
  "Update content in publish static directory."
  (delete-directory blog-publish-static-directory t)
  (copy-directory blog-static-directory blog-publish-static-directory))

(defun mono/blog-update-static-pages (&rest _)
  "Update static pages content."
  ;; this header is inserted into the <head> section of every page:
  (setq org-static-blog-page-header
	(f-read-text (concat blog-root-directory "/public/header.html") 'utf-8))
  ;; this preamble is inserted at the beginning of the <body> of every page:
  (setq org-static-blog-page-preamble
	(f-read-text (concat blog-root-directory "/public/preamble.html") 'utf-8))
  ;; this postamble is inserted at the end of the <body> of every page:
  (setq org-static-blog-page-postamble
	(f-read-text (concat blog-root-directory "/public/postamble.html") 'utf-8))
  ;; this HTML code is inserted into the index page between the
  ;; preamble and the blog posts
  (setq org-static-blog-index-front-matter
	(f-read-text (concat blog-root-directory "/public/title.html") 'utf-8)))


(advice-add 'org-static-blog-publish :before #'mono/blog-update-static-pages)
(advice-add 'org-static-blog-publish :after #'mono/blog-update-statics)

(provide 'mono-blog)

;;; mono-blog.el ends here
