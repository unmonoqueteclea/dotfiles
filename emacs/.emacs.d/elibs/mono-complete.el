;;; mono-complete.el --- completion packages -*- lexical-binding: t -*-

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
;;  In Emacs, one of the most important features of the user interface
;;  is the way in which the user makes selections from lists of items.
;;  Examples: opening files, switching buffers, etc
;;  Interesting video: https://www.youtube.com/watch?v=J0OaRy85MOo
;; 

;;; Code:

(require 'mono-base-package)
(require 'mono-base-theme) ;; all-the-icons is defined there

;; TODO Review this post:
;; https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
;; TODO Review this post:
;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/

;; some configurations recommended by Vertico
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)

;; Vertico provides a performant and minimalistic vertical completion
;; UI based on the default completion system. The main focus of
;; Vertico is to provide a UI which behaves correctly under all
;; circumstances. By reusing the built-in facilities system, Vertico
;; achieves full compatibility with built-in Emacs completion commands
;; and completion tables.  See https://github.com/minad/vertico
(use-package vertico
  :commands vertico-mode
  :defines vertico-count
  :init (vertico-mode)
  ;; default vertico-count is 10 but we want to show more candidates
:config (setq vertico-count 20))

;; this package provides an orderless completion style that divides
;; the pattern into space-separated components, and matches candidates
;; that match all of the components in any order.
;; See https://github.com/oantolin/orderless
(use-package orderless
  :after vertico
  :functions vertico--remote-p
  :custom (completion-styles '(orderless))
  :config
  ;; in combination with Orderless, hostnames are not made available for
  ;; completion after entering /ssh:. In order to avoid this problem,
  ;; the basic completion style should be specified for the file
  ;; completion category.
  ;; See https://github.com/minad/vertico#tramp-hostname-completion
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-styles '(orderless)
        completion-category-overrides
        '((file (styles basic-remote partial-completion)))))


;; this package provides marginalia-mode which adds additional
;; information to the minibuffer completions. Marginalia are marks or
;; annotations placed at the margin of the page of a book or in this
;; case helpful colorful annotations placed at the margin of the
;; minibuffer for your completion candidates.
;; See https://github.com/minad/marginalia
(use-package marginalia
  :demand t
  :commands marginalia-mode
  :config (marginalia-mode 1))

;; add icons to completion candidates using the built in completion
;; metadata functions.
;; see https://github.com/MintSoup/all-the-icons-completion
(use-package all-the-icons-completion)
(all-the-icons-completion-mode)


;; Consult provides practical commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select
;; an item from a list of candidates.  Consult is fully compatible
;; with completion systems based on the standard Emacs completing-read
;; API, notably the default completion system, Vertico,
;; Icomplete/Icomplete-vertical, Selectrum, Embark and Mct.
;; See https://github.com/minad/consult
(use-package consult
  :demand t
  :bind
  (("C-x b" . consult-buffer)
   ("C-s" . consult-line)
   ("M-s" . consult-line-multi)
   ("M-y" . consult-yank-from-kill-ring)
   ("M-g  M-g" . consult-goto-line)
   ("C-c b" . consult-bookmark)
   ("C-c o" . consult-outline)
   ("C-c f" . consult-focus-lines)
   ("C-c l" . consult-find)
   ("C-c i" . consult-imenu-multi)
   ("C-c g" . consult-ripgrep)))

(setq consult-ripgrep-args
      "rg --null -uu --line-buffered --color=never --max-columns=1000
          --glob !{.git,.mypy_cache,.egg-info,.pytest_cache,*.egg-info,node_modules/*}
          --path-separator /   --smart-case --no-heading --line-number .")


;; TODO Check docs and improve configuration of corfu

;; Corfu enhances completion at point with a small completion
;; popup. The current candidates are shown in a popup below or above
;; the point. Corfu is the minimalistic completion-in-region
;; counterpart of the Vertico minibuffer UI.
(use-package corfu
  :demand t
  :functions global-corfu-mode
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :config
  (add-hook 'corfu-mode-hook #'corfu-doc-mode)
  (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
  (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up))  ;; corfu-previous

;; this package provides a sort of right-click contextual menu for
;; Emacs, accessed through the embark-act command (which you should
;; bind to a convenient key), offering you relevant actions to use on a
;; target determined by the context
;; See https://github.com/oantolin/embark
;; TODO Read embark docs and improve configuration
(use-package embark
  :defines embark-act embark-dwim
  :functions embark-prefix-help-command
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("M-," . embark-dwim))        ;; good alternative: M-.
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Allow edition of grep results
;; A common workflow for "find and replace" in a project is:
;;
;; [C-c g] For consult-ripgrep
;; [C-c . E]  For executing embark export to see results in a grep buffer
;; [C-c C-p] In the new grep buffer to active wgrep
(use-package wgrep :demand t)

;; TODO Explore writing linters such us proselint artsbollock and langtool

(provide 'mono-complete)

;;; mono-complete.el ends here
