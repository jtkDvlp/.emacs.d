(use-package
  flycheck
  :diminish
  flycheck-mode

  :hook
  (prog-mode . flycheck-mode-on-safe)

  :config
  (set-default 'flycheck-disabled-checkers '(sass)))

(provide 'init-checking)
