;;; salient-theme.el --- Theme, fonts, look-and-feel, etc  -*- lexical-binding: t; -*-

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

;;; Ligatures
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   'prog-mode
   '(; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Theme
(use-package modus-themes
  ;; https://protesilaos.com/emacs/modus-themes
  :ensure t

  :custom
  (modus-themes-headings '((1 . (rainbow overline background 1.2))
                           (2 . (rainbow overline background 1.1))
                           (t . (rainbow semibold))))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-prompts nil)
  (modus-themes-diffs 'desaturated)
  (modus-themes-links '(color))
  (modus-themes-org-blocks 'gray-background)

  :config
  (custom-set-faces
   '(default ((t (:family "Jetbrains Mono" :height 140)))))

  :init
  (modus-themes-select 'modus-operandi))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (modus-themes-select 'modus-operandi))
    ('dark (modus-themes-select 'modus-vivendi))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(provide 'salient-theme)
;;; salient-theme.el ends here
