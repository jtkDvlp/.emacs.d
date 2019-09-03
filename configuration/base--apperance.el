(use-package
  spaceline
  :demand t
  :config
  (require 'spaceline-segments)
  (setq powerline-default-separator 'wave)

  (spaceline-compile
    '((buffer-id
       :priority 99
       :when (and buffer-file-name (buffer-modified-p))
       :face highlight-face)
      (buffer-id
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
  :hook (prog-mode . linum-mode)
  :config (setq linum-format "%3d"))

(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  beacon
  :demand t
  :diminish beacon-mode
  :config
  (setq beacon-color "#5D4D7A")
  (beacon-mode))

(use-package
  smooth-scrolling
  :demand t
  :config
  (smooth-scrolling-mode t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(custom-set-variables
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-echo-area-message t)
 '(truncate-partial-width-windows nil)
 '(echo-keystrokes 0.02)
 '(query-replace-highlight t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t))

(set-default 'truncate-lines t)
(set-default 'indent-tabs-mode nil)

(show-paren-mode 1)
(custom-set-variables '(show-paren-delay 0))

(custom-set-variables '(frame-title-format (quote ("%b"))))

(load-theme 'spacemacs-dark t)
(toggle-frame-maximized)

(provide 'base--apperance)
