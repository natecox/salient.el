;;; salient-vc.el --- Version control, etc           -*- lexical-binding: t; -*-

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

;;;; Version control

(use-package emacs
  :custom (setq project-vc-merge-submodules nil))

(use-package magit
  ;; https://github.com/magit/magit
  :ensure t
  :bind (("C-c g s" . magit-status))
  :hook ((git-commit-mode . (lambda () (set-fill-column 72))))

  :custom
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
  (git-commit-summary-max-length 50))

(use-package magit-extras
  :after magit)

(use-package forge
  ;; https://github.com/magit/forge
  :ensure t
  :after magit
  :config (push '("git.innova-partners.com" "git.innova-partners.com/api/v3" "git.innova-partners.com" forge-github-repository) forge-alist))

(use-package diff-hl
  ;; https://github.com/dgutov/diff-hl
  :ensure t
  :after magit
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode))


(provide 'salient-vc)
;;; salient-vc.el ends here
