;;; salient-discoverability.el --- Context menus, etc  -*- lexical-binding: t; -*-

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

(use-package hydra
  :ensure t)

(use-package major-mode-hydra
  :ensure t
  :demand t
  :after hydra
  :diminish
  :bind ([s-return] . major-mode-hydra))

(use-package pretty-hydra)

(use-package discover
  :ensure t
  :diminish
  :config (global-discover-mode 1))

(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))


(provide 'salient-discoverability)
;;; salient-discoverability.el ends here
