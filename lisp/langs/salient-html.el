;;; salient-html.el --- HTML programming             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nathan Cox

;; Author: Nathan Cox <nate@natecox.dev>
;; Keywords:

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

(use-package html-mode
  :hook (html-mode . eglot-ensure))

(use-package emmet-mode
  :hook (html-mode . emmet-mode)
  :ensure t)

;; (use-package web-mode
;;   :ensure t

;;   :custom
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-code-indent-offset 2)
;;   (web-mode-enable-css-colorization t)
;;   (web-mode-enable-html-entities-fontification t)
;;   (web-mode-extra-snippets
;;    '(("erb" . (("content_for" . "<% content_for :| do %>\n\n<% end %>")
;;                ("content_for_if" . "<% if content_for?(:|) %>\n<% yield : %>\n<% end %>")
;;                ("var" . "<%= :| %>")))))

;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;;   (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode)))


(provide 'salient-html)
;;; salient-html.el ends here
