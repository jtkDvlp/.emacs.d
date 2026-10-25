;;; init-appearance.el --- Theme and visual aids -*- lexical-binding: t; -*-

;;; Commentary:

;; The theme, the spaceline mode line, line numbers, highlighting of
;; parens, the current line, colours, delimiters and keywords like
;; `WATCHOUT', prettified symbols, font size steps and the frame
;; title.

;;; Code:

(use-package
  spaceline
  :demand t
  :config
  (require 'spaceline-segments)
  (setq powerline-default-separator 'arrow)

  (spaceline-define-segment ide-window
    "IDE-Window marker."
    (let* ((window
            (selected-window))

           (ide-window?
            (window-parameter window 'ide)))

      (when ide-window?
        "IDE")))

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

(setopt frame-title-format
        '((:eval (when (fboundp 'projectile-project-root)
                   (when (projectile-project-root)
                     (concat (projectile-project-name) " - "))))
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
  :requires smartrep
  :config
  (smartrep-define-key
      global-map
      "M-+ f"
    '(("#" . zoom/default)
      ("+" . zoom/inc)
      ("-" . zoom/dec)
      ("1" . zoom/custom-1)
      ("2" . zoom/custom-2)
      ("3" . zoom/custom-3))))

(provide 'init-appearance)

;;; init-appearance.el ends here
