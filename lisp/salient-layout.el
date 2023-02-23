;;; salient-layout.el --- Windows, frames, tabs, buffers, etc  -*- lexical-binding: t; -*-

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

;;; Buffer management

(use-package ibuffer
  :commands ibuffer-find-file
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;;; Tab bar

(use-package tab-bar
  :if (< 26 emacs-major-version)

  :bind
  (("s-{" . tab-bar-switch-to-prev-tab)
   ("s-}" . tab-bar-switch-to-next-tab)
   ("s-w" . tab-bar-close-tab)
   ("s-n" . tab-bar-new-tab))

  :config
  (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
  (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (setq tab-bar-select-tab-modifiers "super"))


(provide 'salient-layout)
;;; salient-layout.el ends here
