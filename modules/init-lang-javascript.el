;;; init-lang-javascript.el --- JavaScript -*- lexical-binding: t; -*-

;;; Commentary:

;; js-ts-mode, the tree-sitter mode, with typescript-language-server
;; through eglot and keys for structural editing.

;;; Code:

(use-package
  js
  :ensure nil

  :init
  (setopt treesit-enabled-modes (cons 'js-ts-mode treesit-enabled-modes))

  ;; NOTE: typescript-language-server, installed outside Emacs, serves
  ;; JavaScript as well.
  :hook
  (js-ts-mode . eglot-ensure)

  :config
  (defun jtk-javascript-transpose-line-up ()
    "Move the current line above the previous one."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode))

  (defun jtk-javascript-transpose-line-down ()
    "Move the current line below the next one."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode))

  ;; NOTE: Statements are lines, so moving one up or down transposes
  ;; lines instead of s-expressions.
  (jtk-structural-editing-bind
   js-ts-mode-map
   '(("C-M-<up>" . jtk-javascript-transpose-line-up)
     ("C-M-<down>" . jtk-javascript-transpose-line-down))))

(provide 'init-lang-javascript)

;;; init-lang-javascript.el ends here
