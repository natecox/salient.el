;;; salient-prog.el --- Programming setup, LSPs, etc  -*- lexical-binding: t; -*-

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

;; Projects

(use-package project
  :bind (:map project-prefix-map
              ("m" . magit-project-status)
              ("v" . vterm))
  :config
  (push '(magit "Magit Status" ?m) project-switch-commands)
  (push '(vterm "vterm" ?v) project-switch-commands))

;; Code analysis tools

(use-package flycheck
  ;; https://www.flycheck.org/en/latest/
  :ensure t
  :diminish
  :config (global-flycheck-mode))

(use-package flycheck-package
  ;; https://github.com/purcell/flycheck-package
  :ensure t
  :after flycheck)

;; Syntax must-haves

;; match paired brackets with colors
(use-package rainbow-delimiters
  ;; https://github.com/Fanael/rainbow-delimiters
  :ensure t
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'lisp-mode 'emacs-lisp-mode 'clojure-mode)
                         (rainbow-delimiters-mode)))))

;; Tree sitter

(use-package tree-sitter
  ;; https://emacs-tree-sitter.github.io
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter
  :ensure t
  :diminish
  :hook ((ruby-mode . tree-sitter-hl-mode)
         (rustic-mode . tree-sitter-hl-mode)
         (haskell-mode . tree-sitter-hl-mode))
  :config (global-tree-sitter-mode))

(use-package tree-sitter-langs
  ;; https://github.com/emacs-tree-sitter/tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eglot
  ;; https://github.com/joaotavora/eglot
  :ensure t)

;; Direnv

(use-package direnv
  :ensure t
  :custom (direnv-always-show-summary nil)
  :config (direnv-mode))


(provide 'salient-prog)
;;; salient-prog.el ends here
