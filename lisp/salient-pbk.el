;;; salient-pbk.el --- Personal Knowledge Base, etc  -*- lexical-binding: t; -*-

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

(use-package org-roam
  :ensure t
  :diminish

  :after (pretty-hydra)
  :bind ("s-r" . 'org-roam-hydra/body)

  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-graph-exclude-matcher '("dailies"))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
     ("c" "clocked" entry "* %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
      :clock-in)))

  :pretty-hydra
  ((:title "Org Roam" :quit-key "q" :exit t)
   ("Slips"
    (("f" org-roam-node-find "Find or create topic")
     ("i" org-roam-node-insert "Insert slip"))
    "Dailies"
    (("d" org-roam-dailies-capture-today "Capture daily note")
     ("D" (lambda () (interactive) (org-roam-dailies-goto-today "d")) "Visit today's slip")
     ("Y" (lambda () (interactive) (org-roam-dailies-goto-yesterday "d")) "Visit yesterday's slip"))
    "Graph"
    (("g" org-roam-graph "Open SVG graph"))
    "Toggle"
    (("B" org-roam-buffer-toggle "Buffer")
     ("L" org-roam "Backlinks")))))
(provide 'salient-pbk)
;;; salient-pbk.el ends here
