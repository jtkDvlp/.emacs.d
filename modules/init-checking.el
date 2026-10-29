;;; init-checking.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

;;; Commentary:

;; flymake in programming modes.  The checks come from the mode (Elisp's
;; byte compiler and checkdoc, shellcheck for shell scripts) or from the
;; language server through eglot.

;;; Code:

(use-package
  flymake
  :ensure nil

  :hook
  (prog-mode . flymake-mode)

  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics))

  :config
  (defvar-keymap jtk-checking-repeat-map
    :doc "Moving between findings; the keys repeat."
    :repeat t
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error))

(provide 'init-checking)

;;; init-checking.el ends here
