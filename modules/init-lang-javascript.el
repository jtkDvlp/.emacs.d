;;; init-lang-javascript.el --- JavaScript -*- lexical-binding: t; -*-

;;; Commentary:

;; js2-mode with refactoring, cross references and the indium
;; debugger.

;;; Code:

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

  ;; NOTE: Statements are lines, so moving one up or down transposes
  ;; lines instead of s-expressions.
  (jtk-structural-editing-bind
   js2-mode-map
   '(("C-M-<up>" . js2-mode-forward-transpose)
     ("C-M-<down>" . js2-mode-backward-transpose))))

(use-package
  js2-refactor
  :after js2-mode
  :diminish js2-refactor-mode
  :hook
  (js2-mode . js2-refactor-mode)
  :bind
  (:map js2-mode-map
        ("M-<down>" . js2r-unwrap)

        ("M-r r" . js2r-rename-var)
        ("M-r o" . js2r-debug-this)
        ("M-r f" . js2r-extract-function)
        ("M-r m" . js2r-extract-method)))

(use-package
  xref-js2
  :after js2-mode
  :init
  ;; NOTE: Buffer-local: the backend answers for every buffer, so on the
  ;; global hook it would take over xref in all modes without their own.
  (defun jtk-xref-js2-enable ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  :hook
  (js2-mode . jtk-xref-js2-enable))

(use-package
  indium
  :after js2-mode
  :diminish indium-interaction-mode
  :hook
  (js2-mode . indium-interaction-mode))

(provide 'init-lang-javascript)

;;; init-lang-javascript.el ends here
