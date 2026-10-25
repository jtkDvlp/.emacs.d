;;; init-structural-editing.el --- Sexp editing -*- lexical-binding: t; -*-

;;; Commentary:

;; smartparens and commands on top of it.  The language modules bind
;; these commands to the same keys in each language.

;;; Code:

(defconst jtk-structural-editing-keys
  '(("C-<left>" . sp-backward-sexp)
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

    ("C-;" . comment-dwim))
  "Keys for editing by s-expressions, the same in every language.
`C-' moves, `M-' changes the structure, `C-M-' transposes or
rewraps; left and right go along a level, up and down between levels.
Language modules apply them with `jtk-structural-editing-bind'.")

(defun jtk-structural-editing-bind (keymap &optional deviations)
  "Bind `jtk-structural-editing-keys' in KEYMAP, then DEVIATIONS.
DEVIATIONS has the same form and holds what a language does
differently, such as transposing lines instead of s-expressions."
  (dolist (binding (append jtk-structural-editing-keys deviations))
    (define-key keymap (kbd (car binding)) (cdr binding))))

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
