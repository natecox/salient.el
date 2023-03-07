;;; salient-core.el --- Config core functions        -*- lexical-binding: t; -*-

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

(setq lexical-binding t)
(setq auto-window-vscroll nil)
(setq max-lisp-eval-depth 2000)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1)))

;;; Set up package archives

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;;; use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package diminish
  ;; https://github.com/myrjola/diminish.el
  :ensure t)

(use-package gnu-elpa-keyring-update
  ;; http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
  :ensure t)

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  :ensure t

  :custom
  (exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-arguments nil)

  :config
  (exec-path-from-shell-initialize))

(use-package emacs
  ;; Configure emacs itself

  :hook ((before-save . delete-trailing-whitespace))

  :bind (("s-SPC" . cycle-spacing))

  :custom
  (completion-styles '(basic substring) "gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html")
  (confirm-nonexistent-file-or-buffer nil)
  (cursor-in-non-selected-windows nil)
  (custom-file (concat user-emacs-directory "custom.el"))
  (custom-safe-themes t "mark all themes as safe, since we can't persist now")
  (dired-listing-switches "-al --group-directories-first")
  (enable-local-variables :all "fix =defvar= warnings")
  (font-lock-maximum-decoration nil)
  (font-lock-maximum-size nil)
  (indicate-empty-lines nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-message t)
  (inhibit-startup-screen t)
  (initial-buffer-choice nil)
  (ispell-program-name "aspell")
  (kill-do-not-save-duplicates t)
  (ring-bell-function 'ignore)
  (sentence-end-double-space nil)
  (temp-buffer-max-height 8)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (user-full-name "Nathan Cox")
  (user-mail-address "nate@natecox.dev")
  (vc-follow-symlinks t)
  (warning-minimum-level :emergency)
  (window-min-height 1)

  :config
  (set-time-zone-rule "/usr/share/zoneinfo.default/America/Los_Angeles")

  (setq-default fill-column 100)
  (auto-fill-mode nil)
  (setq frame-title-format nil)

  ;; Mouse active in terminal
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

  ;; No scroll bars
  (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; No toolbar
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; No menu bar
  (if (display-graphic-p) (menu-bar-mode t) (menu-bar-mode -1))

  ;; Navigate windows using shift+direction
  (windmove-default-keybindings)

  (when (featurep 'ns)
    (defun ns-raise-emacs ()
      "Raise Emacs."
      (ns-do-applescript "tell application \"Emacs\" to activate"))
    (defun ns-raise-emacs-with-frame (frame)
      "Raise Emacs and select the provided frame."
      (with-selected-frame frame
        (when (display-graphic-p)
          (ns-raise-emacs))))
    (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
    (when (display-graphic-p)
      (ns-raise-emacs)))

  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier 'hyper
        ns-use-native-fullscreen t)

  ;; Make sure clipboard works properly in tty mode on OSX
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (when (and (not (display-graphic-p))
             (eq system-type 'darwin))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

  ;; Size of temporary buffers
  (temp-buffer-resize-mode)

  ;; Buffer encoding
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  ;; Default shell in term
  (unless (eq system-type 'windows-nt)
    (setq-default shell-file-name "/bin/zsh")
    (setq explicit-shell-file-name "/bin/zsh"))

  ;; Kill term buffer when exiting
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
    (if (memq (process-status proc) '(signal exit))
        (let ((buffer (process-buffer proc)))
          ad-do-it
          (kill-buffer buffer))
      ad-do-it))
  (ad-activate 'term-sentinel)

  ;; write over selected text on input... like all modern editors do
  (delete-selection-mode t)

  (show-paren-mode)

  ;; stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; enable winner mode globally for undo/redo window layout changes
  (winner-mode t)

  ;; clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t))

(use-package tramp
  ;; customize tramp default behavior
  :custom (tramp-default-method "ssh"))


(use-package recentf
  ;; keep package files out of recentf
  :config
  (add-to-list 'recentf-exclude "\\elpa")
  (add-to-list 'recentf-exclude "^/private")
  (recentf-mode 1))

(provide 'salient-core)

;;; salient-core.el ends here
