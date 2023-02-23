;;; salient.el --- Nate's emacs config               -*- lexical-binding: t; -*-

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

(setq load-prefer-newer t)

(defvar salient-dir (file-name-directory load-file-name)
  "Root directory for salient.")

(add-to-list 'load-path (expand-file-name "lisp" salient-dir))
(add-to-list 'load-path (expand-file-name "lisp/langs" salient-dir))

;; Core functionality, required
(require 'salient-core)

;; Modules, should be optional and isolated
(require 'salient-completion)
(require 'salient-discoverability)
(require 'salient-editor)
(require 'salient-layout)
(require 'salient-orgmode)
(require 'salient-theme)
(require 'salient-vc)

(require 'salient-prog)

(require 'salient-clojure)
(require 'salient-haskell)
(require 'salient-html)
(require 'salient-javascript)
(require 'salient-lisp)
(require 'salient-nix)
(require 'salient-python)
(require 'salient-ruby)
(require 'salient-rust)
(require 'salient-yaml)

(provide 'salient)
;;; salient.el ends here
