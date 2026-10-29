;;; init-lsp.el --- Language servers -*- lexical-binding: t; -*-

;;; Commentary:

;; eglot, the client for language servers.  It feeds what the other
;; modules set up for every language: jumping (xref, M-j), documentation
;; (eldoc, C-h h), completion (corfu) and checking (flymake).  Which
;; language starts which server is up to the language modules; the
;; servers themselves are installed outside Emacs.  Code lenses, as
;; clojure-lsp's count of references, show at the end of their line.

;;; Code:

(use-package
  eglot
  :ensure nil

  :custom
  ;; NOTE: A server stops once its last buffer is closed.
  (eglot-autoshutdown t)
  ;; NOTE: The log of every message to and from the server costs time on
  ;; large answers; `eglot-events-buffer' shows it once switched back on.
  (eglot-events-buffer-config '(:size 0 :format full))
  ;; NOTE: No hint that code actions are at hand: clojure-lsp offers some
  ;; nearly everywhere, so the hint marked every line.  M-r a lists them.
  (eglot-code-action-indications nil)

  :bind
  (:map eglot-mode-map
        ("M-r r" . eglot-rename)
        ("M-r a" . eglot-code-actions)
        ("M-r o" . eglot-code-action-organize-imports)
        ("M-r =" . eglot-format)))

(use-package
  jtk-lsp-lens
  :ensure nil
  :hook
  (eglot-managed-mode . jtk-lsp-lens-enable-maybe))

(provide 'init-lsp)

;;; init-lsp.el ends here
