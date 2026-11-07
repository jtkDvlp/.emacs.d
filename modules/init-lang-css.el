;;; init-lang-css.el --- Style sheets -*- lexical-binding: t; -*-

;;; Commentary:

;; CSS in css-ts-mode (init-tree-sitter) and SCSS in Emacs' own
;; scss-mode, both from css-mode.el.

;;; Code:

;; NOTE: SCSS has no tree-sitter mode in Emacs, and the one grammar for it
;; has had no commit since 2022; scss-mode derives from css-mode and
;; shares its settings.
(use-package
  css-mode
  :ensure nil
  :custom
  (css-indent-offset 2))

(provide 'init-lang-css)

;;; init-lang-css.el ends here
