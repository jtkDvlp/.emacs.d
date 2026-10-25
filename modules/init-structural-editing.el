;;; init-structural-editing.el --- Sexp editing -*- lexical-binding: t; -*-

;;; Commentary:

;; smartparens and commands on top of it.  The language modules bind
;; these commands to the same keys in each language.

;;; Code:

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

(provide 'init-structural-editing)

;;; init-structural-editing.el ends here
