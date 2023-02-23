;;; salient-lisp.el --- Lisp programming             -*- lexical-binding: t; -*-

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

(use-package paredit
  :ensure t
  :hook ((lisp-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (clojure-mode . enable-paredit-mode)))

(use-package rainbow-blocks
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-blocks-mode)
         (clojure-mode . rainbow-blocks-mode)
         (elisp-mode . rainbow-blocks-mode)))


(provide 'salient-lisp)
;;; salient-lisp.el ends here
