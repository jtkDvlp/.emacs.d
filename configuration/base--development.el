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

  :hook
  ((prog-mode . smartparens-mode)
   (cider-repl-mode . smartparens-mode)))

(use-package
  paxedit
  :diminish paxedit-mode

  :hook
  ((prog-mode . paxedit-mode)
   (cider-repl-mode . paxedit-mode)))

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
  :defer t)

(use-package
  yasnippet
  :diminish yas-minor-mode
  :hook
  ((prog-mode . yas-minor-mode-on)
   (cider-repl-mode . yas-minor-mode-on))

  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)
  
  :bind*
  (:map yas-minor-mode-map
        ("C-c" . nil)))

(use-package
  prog-mode
  :ensure nil

  :config
  (defun enable-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          '(("lambda" . 955) ; λ
            ("fn" . 955) ; λ
            ("sum" . 8721))) ; ∑
    (prettify-symbols-mode))
  
  :hook
  ((prog-mode . enable-prettify-symbols)
   (cider-repl-mode . enable-prettify-symbols)))

(use-package
  highlight-symbol
  :diminish highlight-symbol-mode
  :hook
  ((prog-mode . highlight-symbol-mode)
   (cider-repl-mode . highlight-symbol-mode))
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
  markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package
  pkg--ide
  :load-path "configuration/"
  :config
  (ide/configure-frame-layout ide/default-layout)

  (defun ide/setup ()
    (interactive)
    (ide/open-buffers ide/default-buffers)))

(setq eval-sexp-fu-flash-face 'success-face
      eval-sexp-fu-flash-duration 0.3
      eval-sexp-fu-flash-error-face 'error-face
      eval-sexp-fu-flash-error-duration 0.5

      eldoc-echo-area-use-multiline-p t)

(provide 'base--development)
