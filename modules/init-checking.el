;;; init-checking.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

;;; Commentary:

;; flycheck in programming modes.

;;; Code:

(use-package
  flycheck
  :diminish
  flycheck-mode

  :hook
  (prog-mode . flycheck-mode-on-safe)

  :config
  (set-default 'flycheck-disabled-checkers '(sass)))

(provide 'init-checking)

;;; init-checking.el ends here
