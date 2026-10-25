;;; init-lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:

;; markdown-mode, with GitHub flavour for READMEs.

;;; Code:

(use-package
  markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init-lang-markdown)

;;; init-lang-markdown.el ends here
