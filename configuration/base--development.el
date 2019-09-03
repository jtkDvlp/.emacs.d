(use-package
  smartparens
  :diminish smartparens-mode

  :config
  ;; TODO: not here!
  (require 'smartparens-html)
  (require 'smartparens-javascript)

  (defun sp-forward-transpose-sexp ()
    (interactive)
    (sp-next-sexp)
    (sp-transpose-sexp)
    (sp-backward-sexp))
  
  (defun sp-backward-transpose-sexp ()
    (interactive)
    (sp-transpose-sexp)
    (sp-backward-sexp)
    (sp-backward-sexp))

  (defun sp-wrap-doublequote ()
    (interactive)
    (sp-prefix-save-excursion
     (sp-wrap-round)
     (sp-rewrap-sexp "\"")))

  :hook (prog-mode . smartparens-mode))

(use-package
  newcomment
  :ensure nil
  :bind
  (("C-;" . comment-dwim)))

(use-package
  aggressive-indent
  :diminish aggressive-indent-mode
  :hook (prog-mode . aggressive-indent-mode))

(use-package
  company
  :diminish (company-mode eldoc-mode)
  :hook (prog-mode . company-mode)
  :config
  (setq company-tooltip-align-annotations t))

(use-package
  company-quickhelp
  :after company
  :hook (prog-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.2))

(use-package
  magit
  :requires (ivy fullframe)
  :bind*
  (("C-v" . nil)
   ("M-v g" . magit-status)
   ("M-v l" . magit-log-current)
   ("M-v L" . magit-log-all))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (fullframe magit-status magit-mode-quit-window)
  (fullframe magit-log-all magit-mode-quit-window)
  (fullframe magit-log-current magit-mode-quit-window))

(use-package
  flycheck
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode-on-safe))

(use-package
  yasnippet-snippets
  :hook (prog-mode . yas-minor-mode-on))

(use-package
  yasnippet
  :diminish yas-minor-mode
  :after yasnippet-snippets)

(use-package
  highlight-symbol
  :diminish highlight-symbol-mode
  :hook (prog-mode . highlight-symbol-mode)
  :config (setq highlight-symbol-idle-delay 0.8))

(use-package
  restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package
  sql
  :commands (sql-mysql)
  :config
  (when (equal system-type 'windows-nt)
    (setq sql-mysql-options '("-C" "-t" "-f" "-n"))))

(use-package
  pkg--ide
  :load-path "configuration/"
  :config
  (ide/configure-frame-layout ide/default-layout)

  (defun ide/setup ()
    (interactive)
    (ide/open-buffers ide/default-buffers)))

(provide 'base--development)
