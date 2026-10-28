;;; init-lang-elisp.el --- Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Keys for structural editing, evaluating and finding definitions,
;; and a helper to dump values while debugging.

;;; Code:

(use-package
  elisp-mode
  :ensure nil

  :mode
  ("\\.el\\'" . emacs-lisp-mode)

  :config
  (jtk-structural-editing-bind emacs-lisp-mode-map)

  :bind
  (:map emacs-lisp-mode-map
        ("M-j d" . find-function-at-point)
        ("M-j f" . find-function-at-point)

        ("C-c C-c" . eval-defun)
        ("C-M-x" . eval-last-sexp)
        ("C-c M-k" . eval-buffer)))

(defun dump (data)
  (setq dump-buffer (get-buffer-create "*dump*"))
  (print data dump-buffer))

(provide 'init-lang-elisp)

;;; init-lang-elisp.el ends here
