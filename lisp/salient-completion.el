;;; salient-completion.el --- Completion frameworks, lists, etc  -*- lexical-binding: t; -*-

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

(use-package consult
  :ensure t
  :bind ("C-x b" . consult-buffer))


(use-package corfu
  ;; https://github.com/minad/corfu

  :ensure t

  :custom
  (corfu-cycle t)

  :init
  (global-corfu-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; add annotations to minibuffers
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("C-M-a" . marginalia-cycle))
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t

  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t))

;; better interface for selecting items from a list
(use-package vertico
  :ensure t
  :init (vertico-mode))


(provide 'salient-completion)
;;; salient-completion.el ends here
