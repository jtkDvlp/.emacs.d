;;; init-appearance.el --- Theme and visual aids -*- lexical-binding: t; -*-

;;; Commentary:

;; The theme, the spaceline mode line, line numbers, highlighting of
;; parens, the current line, colours, delimiters and keywords like
;; `WATCHOUT', prettified symbols, font size steps and the frame
;; title.

;;; Code:

(require 'jtk-project)

(use-package
  spaceline
  :demand t
  :config
  (require 'spaceline-segments)
  (setq powerline-default-separator 'arrow)

  (spaceline-define-segment ide-window
    "Marks the side windows of the IDE-like layout."
    (when (window-parameter (selected-window) 'window-side)
      "IDE"))

  (spaceline-compile
    '((buffer-id
       :priority 99
       :when (and active buffer-file-name (buffer-modified-p))
       :face highlight-face)
      ((buffer-id "~")
       :priority 99
       :when (and (not active) buffer-file-name (buffer-modified-p))
       :face highlight-face)
      ((buffer-id)
       :priority 99
       :when (not (and buffer-file-name (buffer-modified-p))))
      (buffer-size
       :priority 60
       :when active)
      (major-mode
       :priority 70)
      ((flycheck-error flycheck-warning flycheck-info)
       :priority 80
       :when active)
      (version-control
       :priority 60)
      (buffer-encoding
       :when (and active vc-mode)
       :priority 50))

    '((minor-modes
       :when active
       :priority 70)
      (ide-window
       :priority 60)
      (line-column
       :separator " | "
       :priority 50
       :when active)
      (buffer-position
       :priority 40
       :when active)))

  (setq-default
   mode-line-format
   '("%e" (:eval (spaceline-ml-main)))))

(use-package
  linum
  :ensure nil
  :hook (prog-mode . linum-mode)
  :config (setq linum-format "%3d"))

(use-package
  rainbow-mode
  :diminish rainbow-mode

  :hook
  ((prog-mode . rainbow-mode)
   (cider-repl-mode . rainbow-mode)))

(use-package
  rainbow-delimiters
  :hook
  ((prog-mode . rainbow-delimiters-mode)
   (cider-repl-mode . rainbow-delimiters-mode)))

(defface error-face
  '((t (:foreground "#CC5353"))) "Red Highlight")

(defface success-face
  '((t (:foreground "yellow" :background "#555577"))) "Green Highlight")

(show-paren-mode 1)

(setopt show-paren-delay 0)

;; NOTE: Evaluated at every redisplay of the title; `project-current'
;; takes 0.015 ms a call (Emacs 31.1, 1000 calls, in and outside a git
;; repository), so it needs no cache.
(setopt frame-title-format
        '((:eval (when-let* ((name (jtk-project-name)))
                   (concat name " - ")))
          "%b"))

(global-hl-line-mode 1)

(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    '(("\\<\\(HACK\\|NOTE\\|FIXME\\|TODO\\|BUG\\|INFO\\|IDEA\\|WATCHOUT\\|WTF\\)"
       1 font-lock-warning-face t)))))

(use-package
  doom-themes
  :demand t
  :config
  (load-theme 'doom-one t))

(use-package
  prog-mode
  :ensure nil

  :config
  (defun enable-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          '(("lambda" . 955) ; λ
            ("fn" . 955) ; λ
            ("sum" . 8721))) ; ∑
    (prettify-symbols-mode))

  :hook
  ((prog-mode . enable-prettify-symbols)
   (cider-repl-mode . enable-prettify-symbols)))

(use-package
  jtk-zoom
  :ensure nil
  :demand t
  :config
  (defvar-keymap jtk-zoom-map
    :doc "Font size of the frame; the keys repeat."
    :prefix t
    :repeat t
    "#" #'jtk-zoom-default
    "+" #'jtk-zoom-inc
    "-" #'jtk-zoom-dec
    "1" #'jtk-zoom-custom-1
    "2" #'jtk-zoom-custom-2
    "3" #'jtk-zoom-custom-3)
  (keymap-set jtk-view-map "f" 'jtk-zoom-map))

(provide 'init-appearance)

;;; init-appearance.el ends here
