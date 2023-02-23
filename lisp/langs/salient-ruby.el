;;; salient-ruby.el --- Ruby programming             -*- lexical-binding: t; -*-

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

(use-package ruby-mode
  :hook (ruby-mode . eglot-ensure))

(use-package bundler
  :ensure t)

(use-package yard-mode
  :ensure t
  :after ruby-mode
  :hook ruby-mode)

(use-package inf-ruby
  :ensure t
  :config (setenv "PAGER" (executable-find "cat")))

(use-package rspec-mode
  :ensure t
  :hook ((after-init . inf-ruby-switch-setup)
         (compilation-filter-hook . inf-ruby-auto-enter))
  :custom
  (compilation-scroll-output t)
  (rspec-primary-source-dirs '("app")))

(use-package rubocop
  :ensure t)


(provide 'salient-ruby)
;;; salient-ruby.el ends here
