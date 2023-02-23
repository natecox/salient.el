;;; salient-orgmode.el --- Org mode, captures, exports, etc  -*- lexical-binding: t; -*-

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

;;;; Orgmode

;; 1. Install macTEX with `brew install cask mactex`
;; 2. Download and install https://amaxwell.github.io/tlutility/
;; 3. Ensure Lato font is installed

(defun nc/org-insert-dwim (&optional arg)
  "Insert an appropriate org item. ARG optional."
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (let ((org-special-cprl-a/e t)
          (below? (unless (equal arg '(4)) '(4))))
      (cond ((org-at-item-p)
             (let ((org-M-RET-may-split-line nil)
                   (org-enable-sort-checkbox nil))
               (when below? (org-end-of-line))
               (org-insert-item (org-at-item-checkbox-p))))
            ((org-before-first-heading-p)
             (org-insert-heading))
            (t
             (org-back-to-heading)
             (if (org-get-todo-state)
                 (org-insert-todo-heading t below?)
               (org-insert-heading below?)))))))

(use-package org
  :after (major-mode-hydra)
  :pin gnu
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c r" . org-refile)
         :map org-mode-map
         ("M-<return>" . nc/org-insert-dwim))

  ;; :hook ((after-init . (lambda () (org-agenda nil "n"))))

  :custom
  (org-ascii-links-to-notes nil)
  (org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'current-window)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (concat org-directory "/notes.org"))
  (org-directory "~/org")
  (org-display-remote-inline-images 'download)
  (org-export-copy-to-kill-ring nil)
  (org-export-headline-levels 2)
  (org-export-with-author nil)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  (org-export-with-toc nil)
  (org-global-properties
   '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")))
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex" "aux" "idx" "log" "out" "toc" "nav"
           "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc"
           "ps" "spl" "bbl" "xdv")))
  (org-latex-compiler "xelatex")
  (org-latex-pdf-process '("latexmk -xelatex -quiet -shell-escape -f %f"))
  (org-log-into-drawer t)
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-return-follows-link nil)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-tag-alist '((:startgrouptag) ("people") (:grouptags) ("{^@.+}") (:endgrouptag)))
  (org-tags-column 80)

  :mode-hydra
  (org-mode
   (:title "Org Mode Commands")
   ("Timestamps"
    (("ts" org-time-stamp "Insert active")
     ("ti" org-time-stamp-inactive "Insert inactive"))))

  :config
  (add-to-list 'exec-path "/Library/TeX/texbin")
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t))

(use-package org
  ;; babel config
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ruby . t)
     (python . t)
     (shell . t))))

(use-package mermaid-mode
  ;; https://github.com/abrochard/mermaid-mode
  :ensure t
  :custom (mermaid-mmdc-location "~/.asdf/shims/mmdc"))

(use-package ob-mermaid
  ;; https://github.com/arnm/ob-mermaid
  :ensure t
  :custom (ob-mermaid-cli-path "~/.asdf/shims/mmdc"))

;;; Exporters

(use-package org-contrib
  :ensure t)

;; add jira format export
(use-package ox-jira
  ;; https://github.com/emacsmirror/ox-jira
  :ensure t
  :after org
  :config (eval-after-load "org" '(progn (require 'ox-jira))))

;; add github flavored markdown export
(use-package ox-gfm
  :ensure t
  :config (eval-after-load "org" '(require 'ox-gfm nil t)))

;; add confluence formatted export
(use-package ox-confluence
  ;; https://github.com/aspiers/orgmode/blob/master/contrib/lisp/ox-confluence.el
  :after org-contrib
  :config (eval-after-load "org" '(require 'ox-contrib nil t)))

(use-package org-tree-slide
  :ensure t)

(provide 'salient-orgmode)
;;; salient-orgmode.el ends here
