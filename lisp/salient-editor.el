;;; salient-editor.el --- Text editing, indentation, etc  -*- lexical-binding: t; -*-

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

;;; Indentation

(use-package emacs
  :custom
  (tab-width 2)
  :config
  (setq-default indent-tabs-mode nil)   ; Always use spaces by default
  (electric-indent-mode +1))

(use-package smart-tabs-mode
  ;; https://www.emacswiki.org/emacs/SmartTabs
  ;; indent with tabs, align with spaces where enabled
  :ensure t
  :config (smart-tabs-insinuate 'ruby))

(use-package highlight-indent-guides
  ;; https://github.com/DarthFennec/highlight-indent-guides
  :ensure t
  :hook ((prog-mode . highlight-indent-guides-mode)
         (taskpaper-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'bitmap)
  (highlight-indent-guides-responsive 'stack))


;;; Text manipulation

(use-package vundo
  :ensure t
  :custom (vundo-glyph-alist vundo-unicode-symbols)
  :bind (("s-/" . #'vundo)))

(use-package expand-region
  :ensure t
  :diminish
  :bind (("s-e" . #'er/expand-region)))

(use-package change-inner
  :ensure t
  :diminish
  :after expand-region
  :bind (("s-i" . #'change-inner)
         ("s-o" . #'change-outer)))

(use-package avy
   ;; https://github.com/abo-abo/avy
  :ensure t
  :bind (("s-t" . 'avy-goto-char-timer)
         ("s-T" . 'avy-goto-line)
         ("C-c C-j" . 'avy-resume))
  :config (avy-setup-default))

(use-package ctrlf
  :ensure t
  :config (ctrlf-mode t))

(use-package hl-line-mode
  ;; use the builtin current-line highlighter
  :hook ((prog-mode) (text-mode) (org-agenda-mode)))

(use-package flyspell-mode
  ;; https://www.emacswiki.org/emacs/FlySpell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))


(provide 'salient-editor)
;;; salient-editor.el ends here
