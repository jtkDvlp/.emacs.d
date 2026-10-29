;;; init-lang-elisp.el --- Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:

;; Keys for structural editing, evaluating and finding definitions,
;; trust in the configuration's own code for checking, and a helper to
;; dump values while debugging.

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
        ("M-j f" . find-function-at-point)

        ("C-c C-c" . eval-defun)
        ("C-M-x" . eval-last-sexp)
        ("C-c M-k" . eval-buffer)))

;; NOTE: flymake byte-compiles only files whose content is trusted, since
;; compiling runs their macros; elsewhere it runs checkdoc alone.  The
;; configuration's own code is trusted, the packages under elpaca/ not.
(let ((root (abbreviate-file-name user-emacs-directory)))
  (setopt trusted-content
          (append (mapcar (lambda (name) (concat root name))
                          '("early-init.el" "init.el" "modules/" "lisp/"))
                  (and (listp trusted-content) trusted-content))))

(defun dump (data)
  (setq dump-buffer (get-buffer-create "*dump*"))
  (print data dump-buffer))

(provide 'init-lang-elisp)

;;; init-lang-elisp.el ends here
