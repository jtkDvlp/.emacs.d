(use-package
  company
  :diminish (company-mode eldoc-mode)
  :hook
  ((prog-mode . company-mode)
   (cider-repl-mode . company-mode))
  :config
  (setq company-tooltip-align-annotations t))

(use-package
  company-quickhelp
  :after company
  :hook
  ((prog-mode . company-quickhelp-mode)
   (cider-repl-mode . company-quickhelp-mode))
  :config (setq company-quickhelp-delay 0.2))

(provide 'init-completion)
