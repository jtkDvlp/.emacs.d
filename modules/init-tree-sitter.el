;;; init-tree-sitter.el --- Tree-sitter modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Which file types open in the tree-sitter based mode instead of the
;; classic one.  Emacs installs a missing grammar after asking (it needs
;; a C compiler, on the Mac the Xcode command line tools).  Modes that
;; carry configuration of their own switch with their language module.

;;; Code:

;; NOTE: tree-sitter parses incrementally, which keeps highlighting and
;; indentation fast in large files.
(setopt treesit-enabled-modes
        '(bash-ts-mode
          css-ts-mode
          dockerfile-ts-mode
          json-ts-mode
          toml-ts-mode
          yaml-ts-mode))

(provide 'init-tree-sitter)

;;; init-tree-sitter.el ends here
