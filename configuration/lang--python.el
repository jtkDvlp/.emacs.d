;; pip install jedi rope flake8 autopep8 yapf black
(use-package
  elpy
  :hook (python-mode . elpy-enable)

  :bind* 
  (:map python-mode-map
        ("C-<left>" . sp-backward-sexp)
	("M-<left>" . sp-forward-barf-sexp)
	("C-M-<left>" . sp-backward-barf-sexp)
	
	("C-<right>" . sp-forward-sexp)
	("M-<right>" . sp-forward-slurp-sexp)
	("C-M-<right>" . sp-backward-slurp-sexp)
	
	("C-<up>" . sp-backward-up-sexp)
	("M-<up>" . sp-convolute-sexp)

	("C-<down>" . sp-down-sexp)
	("M-<down>" . sp-raise-sexp)
	
	("C-M-<up>" . sp-backward-transpose-sexp)
	("C-M-<down>" . sp-forward-transpose-sexp)
	
	("M-\"" . sp-wrap-doublequote)
	("C-(" . sp-wrap-round)
	("M-[" . sp-wrap-square)
	("M-{" . sp-wrap-curly)
	
	("C-M-(" . sp-rewrap-sexp)

	("C-k" . sp-kill-sexp)
	("C-S-K" . sp-unwrap-sexp)
	
	("M-k" . sp-splice-sexp-killing-forward)
	("M-K" . sp-splice-sexp-killing-backward)
	("C-M-k" . sp-splice-sexp-killing-around)

	("C-;" . comment-dwim)
        
        ("M-j d" . elpy-goto-definition)
        ("M-J" . pop-tag-mark))
  
  :config
  (require 'smartparens-python)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq
   elpy-modules (delq 'elpy-module-flymake elpy-modules)
   elpy-get-info-from-shell t))

(provide 'lang--python)
