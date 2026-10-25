;;; init-core.el --- Basic behaviour of Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings of Emacs itself that belong to no topic of their own:
;; machine-specific settings from local.el, PATH, backups and lock
;; files, encoding, reverting changed files, scrolling, prompts.
;; Anything tied to one package or topic goes into that topic's
;; module.

;;; Code:

;; local.el is not versioned: it holds what differs between machines.
(load (expand-file-name "local.el" user-emacs-directory) 'noerror)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize)
  ;; NOTE: Only after reading PATH: `exec-path-from-shell' asks
  ;; `shell-file-name', and PATH is set up for the login shell.
  (setq shell-file-name "/bin/bash"))

(global-so-long-mode 1)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(let ((n 0))
  (setq scroll-step 1)
  (setq scroll-conservatively n)
  (setq scroll-margin n))

(setopt inhibit-startup-screen t
        inhibit-startup-message t
        truncate-partial-width-windows nil
        echo-keystrokes 0.02
        query-replace-highlight t
        next-error-highlight t
        next-error-highlight-no-select t)

(set-default 'truncate-lines nil)

(set-default 'indent-tabs-mode nil)

(setopt transient-mark-mode t
        highlight-nonselected-windows t
        shift-select-mode t
        select-enable-clipboard t
        initial-major-mode 'fundamental-mode
        initial-buffer-choice "*empty*"
        ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setopt uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"
        backup-inhibited t
        delete-by-moving-to-trash nil
        compilation-ask-about-save nil
        confirm-nonexistent-file-or-buffer nil
        require-final-newline t)

(set-language-environment 'utf-8)

(set-default-coding-systems 'utf-8)

(setq locale-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(set-selection-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(global-auto-revert-mode 1)

(setopt global-auto-revert-non-file-buffers t)

(auto-compression-mode t)

(setq eval-sexp-fu-flash-face 'success-face
      eval-sexp-fu-flash-duration 0.3
      eval-sexp-fu-flash-error-face 'error-face
      eval-sexp-fu-flash-error-duration 0.5

      eldoc-echo-area-use-multiline-p t)

(provide 'init-core)

;;; init-core.el ends here
