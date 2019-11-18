(use-package
  tide

  :after (typescript-mode company flycheck)

  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save . tide-formater-before-save))

  :config
  (setq tide-completion-detailed t
        tide-always-show-documentation t

        typescript-indent-level 2)
  
  :bind*
  (:map tide-mode-map
	("C-<left>" . sp-backward-sexp)
  	("M-<left>" . sp-forward-barf-sexp)
  	("C-M-<left>" . sp-backward-barf-sexp)
	
  	("C-<right>" . sp-forward-sexp)
  	("M-<right>" . sp-forward-slurp-sexp)
  	("C-M-<right>" . sp-backward-slurp-sexp)

  	("C-<up>" . sp-backward-up-sexp)
  	;; ("M-<up>" . sp-convolute-sexp)

  	("C-<down>" . sp-down-sexp)
	
  	("C-M-<up>" . js2-mode-forward-transpose)
  	("C-M-<down>" . js2-mode-backward-transpose)
	
  	;; ("M-\"" . sp-wrap-doublequote)
  	("C-(" . sp-wrap-round)
  	("M-[" . sp-wrap-square)
  	("M-{" . sp-wrap-curly)
	
  	;; ("C-M-(" . sp-rewrap-sexp)

  	("C-k" . sp-kill-sexp)
  	("C-S-K" . sp-unwrap-sexp)
	
  	("M-k" . sp-splice-sexp-killing-forward)
  	;; ("M-K" . sp-splice-sexp-killing-backward)
  	("C-M-k" . sp-splice-sexp-killing-around)

        ("M-j d" . tide-jump-to-definition)
        ("M-J" . tide-jump-back)

        ("C-h h" . tide-documentation-at-point)

        ("M-r r" . tide-rename-symbol)
        ("M-r f" . tide-rename-file)

        ("C-C M-j" . tide-restart-server)))

(use-package
  npm-mode

  :commands
  (npm-mode-npm-init
   npm-mode-npm-install
   npm-mode-npm-run))

(provide 'lang--typescript)
