;;; init-lsp.el --- Language servers -*- lexical-binding: t; -*-

;;; Commentary:

;; eglot, the client for language servers.  It feeds what the other
;; modules set up for every language: jumping (xref, M-j), documentation
;; (eldoc, C-h h), completion (corfu) and checking (flymake).  Which
;; language starts which server is up to the language modules; the
;; servers themselves are installed outside Emacs.

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

  :bind
  ;; NOTE: Capitals, since clj-refactor holds most lower-case letters
  ;; after M-r in Clojure buffers.
  (:map eglot-mode-map
        ("M-r R" . eglot-rename)
        ("M-r A" . eglot-code-actions)
        ("M-r O" . eglot-code-action-organize-imports)
        ("M-r =" . eglot-format)))

(provide 'init-lsp)

;;; init-lsp.el ends here
