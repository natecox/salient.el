;;; salient-pkb.el --- Personal Knowledge Base, etc  -*- lexical-binding: t; -*-

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
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>")
      :clock-in)))

  :pretty-hydra
  ((:title "Org Roam" :quit-key "q" :exit t)
   ("Slips"
    (("f" org-roam-node-find "Find or create topic")
     ("i" org-roam-node-insert "Insert slip"))
    "Dailies"
    (("d" org-roam-dailies-capture-today "Capture daily note")
     ("y" org-roam-dailies-capture-yesterday "Capture daily note (yesterday)")
     ("t" org-roam-dailies-capture-tomorrow "Capture daily note (tomorrow)")
     ("c" org-roam-dailies-capture-date "Select a date to capture")
     ("D" org-roam-dailies-goto-today "Visit today's slip")
     ("Y" org-roam-dailies-goto-yesterday "Visit daily slip (yesterday)")
     ("T" org-roam-dailies-goto-tomorrow "Visit daily slip (tomorrow)")
     ("C" org-roam-dailies-goto-date "Select a date to visit"))
    "Graph"
    (("g" org-roam-graph "Open SVG graph"))
    "Toggle"
    (("B" org-roam-buffer-toggle "Buffer")
     ("L" org-roam "Backlinks")))))

(provide 'salient-pkb)
;;; salient-pkb.el ends here
