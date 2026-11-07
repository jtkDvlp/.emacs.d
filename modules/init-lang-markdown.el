;;; init-lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:

;; md-mode for Markdown, on trial: styled while editable, and C-c C-v
;; renders the buffer in place, tables included, without a browser.
;; markdown-mode stays at hand (M-x markdown-mode, gfm-mode).

;;; Code:

(use-package
  textui
  :ensure (:host github :repo "yibie/textui"))

(use-package
  md-mode
  :ensure (:host github :repo "yibie/md-mode")
  :mode
  ("\\.\\(md\\|markdown\\)\\'" . md-mode)

  ;; NOTE: Rendering is to Markdown what evaluating the buffer is to
  ;; code, so it takes that key as well as md-mode's own C-c C-v.
  :bind
  (:map md-mode-map
        ("C-c M-k" . md-mode-toggle-markup)))

(use-package
  markdown-mode
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(provide 'init-lang-markdown)

;;; init-lang-markdown.el ends here
