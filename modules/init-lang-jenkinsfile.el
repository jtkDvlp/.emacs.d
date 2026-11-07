;;; init-lang-jenkinsfile.el --- Jenkins pipelines -*- lexical-binding: t; -*-

;;; Commentary:

;; jenkinsfile-mode for declarative pipelines, on top of groovy-mode.

;;; Code:

;; NOTE: Neither package has changed since 2023; both are small, and Emacs
;; has nothing for Groovy, with or without tree-sitter.
(use-package
  jenkinsfile-mode
  :mode
  ("\\(?:\\`\\|/\\)Jenkinsfile\\(?:\\.[^/]*\\)?\\'" . jenkinsfile-mode))

(provide 'init-lang-jenkinsfile)

;;; init-lang-jenkinsfile.el ends here
