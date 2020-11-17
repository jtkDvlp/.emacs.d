(use-package
  js2-mode
  :mode
  ("\\.js\\'")
  :hook
  (js2-mode . js2-imenu-extras-mode)

  :config
  (defun js2-mode-backward-sexp (&optional n)
    (interactive)
    (let ((n (if n n 1)))
      (js2-mode-forward-sexp (* n -1))))

  (defun js2-mode-forward-transpose ()
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode))

  (defun js2-mode-backward-transpose ()
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode))
  
  :bind*
  (:map js2-mode-map
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
  	("C-M-k" . sp-splice-sexp-killing-around)))

(use-package
  js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook
  (js2-mode . js2-refactor-mode)
  :bind*
  (:map js2-mode-map
  	("M-<down>" . js2r-unwrap)
	
	("M-r r" . js2r-rename-var)
	("M-r o" . js2r-debug-this)
	("M-r f" . js2r-extract-function)
	("M-r m" . js2r-extract-method)))

(use-package
  xref-js2
  :after js2-mode
  :hook
  (xref-backend-functions . xref-js2-xref-backend)
  :bind*
  (:map js2-mode-map
	("M-j d" . xref-find-definitions)
	("M-j r" . xref-find-references)
	("M-J " . xref-pop-maker-stack)))

(use-package
  indium
  :after js2-mode
  :diminish indium-interaction-mode
  :hook
  (js2-mode . indium-interaction-mode))

(provide 'lang--javascript)
