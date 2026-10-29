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

    ("C-M-<up>" . jtk-structural-editing-transpose-backward)
    ("C-M-<down>" . jtk-structural-editing-transpose-forward)

    ("M-\"" . jtk-structural-editing-wrap-doublequote)
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
  ;; NOTE: The rules for each language, loaded along with its mode:
  ;; among them, ' and ` stay unpaired in Lisp.
  (require 'smartparens-config)

  (defun jtk-structural-editing-transpose-forward ()
    "Move the s-expression after point past the next one."
    (interactive)
    (sp-next-sexp)
    (sp-transpose-sexp)
    (sp-backward-sexp))

  (defun jtk-structural-editing-transpose-backward ()
    "Move the s-expression before point past the previous one."
    (interactive)
    (sp-transpose-sexp)
    (sp-backward-sexp)
    (sp-backward-sexp))

  (defun jtk-structural-editing-wrap-doublequote ()
    "Wrap the s-expression at point in double quotes."
    (interactive)
    (sp-prefix-save-excursion
     (sp-wrap-round)
     (sp-rewrap-sexp "\"")))

  :hook
  ((prog-mode . smartparens-mode)
   (cider-repl-mode . smartparens-mode)))

(provide 'init-structural-editing)

;;; init-structural-editing.el ends here
