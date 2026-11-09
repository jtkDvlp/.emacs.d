;;; init-appearance.el --- Theme and visual aids -*- lexical-binding: t; -*-

;;; Commentary:

;; The theme, the doom-modeline mode line, line numbers, highlighting of
;; parens, the current line, colours, delimiters and keywords like
;; `WATCHOUT', prettified symbols, font size steps and the frame
;; title.

;;; Code:

(require 'jtk-project)

;; NOTE: Knows flymake, eglot, the project and the branch by itself.
;; Its icons need a Nerd Font, once: M-x nerd-icons-install-fonts.
(use-package
  doom-modeline
  :demand t
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (doom-modeline-mode 1))

;; NOTE: Marks the side windows of the IDE-like layout (init-windows).
;; While a mode line is drawn, `selected-window' is the window it
;; belongs to.
(defvar jtk-appearance-side-window-marker
  '(:eval (when (window-parameter (selected-window) 'window-side) "IDE "))
  "Mode line construct that marks a side window.")

(add-to-list 'mode-line-misc-info 'jtk-appearance-side-window-marker)
(put 'jtk-appearance-side-window-marker 'risky-local-variable t)

;; NOTE: Drawn by the display engine, so nearly free: a key press in a
;; Clojure file of 7,700 lines takes 3.5 ms with it and 560 ms with the
;; obsolete linum-mode (Emacs 31.1, cloud, batch).
(use-package
  display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :custom (display-line-numbers-width 3))

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
  (defun jtk-appearance-prettify-symbols ()
    "Show lambda and fn as λ and sum as ∑ in the current buffer."
    (setq-local prettify-symbols-alist
                (append '(("lambda" . ?λ) ("fn" . ?λ) ("sum" . ?∑))
                        prettify-symbols-alist))
    (prettify-symbols-mode 1))

  :hook
  ((prog-mode . jtk-appearance-prettify-symbols)
   (cider-repl-mode . jtk-appearance-prettify-symbols)))

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
