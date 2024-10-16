;;; mono-ai.el --- AI-related packages -*- lexical-binding: t -*-

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
;;  LLM and other AI-related packages.

;;; Code:

(require 'mono-base-package)
(require 'mono-base-definitions)

;; I don´t have the secret module in all environments
(when (file-exists-p "elibs/mono-secret.el")
  (require 'mono-secret))

;; https://github.com/karthink/gptel?
(use-package gptel
  :config
  (setq gptel-model "gemini-pro"
        gptel-backend (gptel-make-gemini "Gemini" :key gemini-api-key :stream t)))


(provide 'mono-ai)

;;; mono-ai.el ends here
