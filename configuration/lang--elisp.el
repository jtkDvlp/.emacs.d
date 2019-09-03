(use-package
  elisp-mode
  :ensure nil

  :mode
  ("\\.el\\'" . emacs-lisp-mode)

  :bind*
  (:map emacs-lisp-mode-map
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

	("M-j f" . find-function-at-point)

	("C-;" . comment-dwim)

	("C-M-x" . eval-defun)
	("C-c M-k" . eval-buffer)))

(provide 'lang--elisp)
