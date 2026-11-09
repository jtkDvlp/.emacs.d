;;; init-lang-markdown.el --- Markdown -*- lexical-binding: t; -*-

;;; Commentary:

;; md-mode for Markdown, on trial: styled while editable, and C-c C-v
;; renders the buffer in place, tables included, without a browser.
;; markdown-mode stays at hand (M-x markdown-mode, gfm-mode).

;;; Code:

(use-package
  textui
  :ensure (:host github :repo "yibie/textui"))

;; WATCHOUT: md-mode sets `truncate-lines' itself on every
;; fontification, so that wide table rows stay reachable by scrolling
;; sideways; `toggle-truncate-lines' lasts only until the next redisplay.
;; md-mode lets lines wrap only under `visual-line-mode'.  An advice
;; rather than a remap, which keys follow but M-x does not.
(defun jtk-markdown-toggle-wrapping (toggle &rest args)
  "In md-mode toggle `visual-line-mode', elsewhere call TOGGLE with ARGS.
Meant as advice around `toggle-truncate-lines'."
  (if (derived-mode-p 'md-mode)
      (visual-line-mode 'toggle)
    (apply toggle args)))

(advice-add 'toggle-truncate-lines :around #'jtk-markdown-toggle-wrapping)

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
