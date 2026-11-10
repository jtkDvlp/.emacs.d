;;; init-appearance.el --- Theme and visual aids -*- lexical-binding: t; -*-

;;; Commentary:

;; The theme, the doom-modeline mode line, line numbers, highlighting of
;; parens, the current line, colours, delimiters and keywords like
;; `WATCHOUT', prettified symbols, font size steps and the frame
;; title.

;;; Code:

(require 'jtk-project)

(defvar nerd-icons-font-family)
(declare-function nerd-icons-install-fonts "nerd-icons")

;; NOTE: early-init only keeps frames from getting a menu bar.  On macOS
;; the menus sit in the system's bar, and a frame that got one after
;; all left them there; switching the mode off clears them for every
;; frame.  The "Emacs" menu itself, as every application's first menu,
;; stays; `ns-auto-hide-menu-bar' would hide the whole bar instead.
(menu-bar-mode -1)

;; NOTE: The icons of the mode line need a Nerd Font.  On a machine
;; without it, the first start in the GUI fetches it from nerd-icons'
;; repository into ~/Library/Fonts, without asking; then it is found and
;; nothing more happens.  Not in batch or a terminal, which have no use
;; for it.
;; WATCHOUT: Asked through `font-family-list', the families actually
;; installed.  With `find-font' the font was never fetched on the Mac,
;; most likely because it answers for a missing family with a stand-in.
(defun jtk-appearance-ensure-icon-font ()
  "Install the font of `nerd-icons' if a graphical display lacks it."
  (require 'nerd-icons)
  (when (and (display-graphic-p)
             (not (member nerd-icons-font-family (font-family-list))))
    (nerd-icons-install-fonts t)
    (message "Installed %s; restart Emacs if icons still show as boxes"
             nerd-icons-font-family)))

;; NOTE: Knows flymake, eglot, the project and the branch by itself.
(use-package
  doom-modeline
  :demand t
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  :config
  (jtk-appearance-ensure-icon-font)
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

;; NOTE: Only where colors are written down; elsewhere it took any word
;; that names a color, or a hex number, for one.
(use-package
  rainbow-mode
  :diminish rainbow-mode

  :hook
  ((css-base-mode web-mode emacs-lisp-mode) . rainbow-mode))

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

;; NOTE: The keywords of comments by what they mean: a fault in the
;; theme's error color, a warning (WATCHOUT, HACK) in its warning color,
;; what is still to do as a keyword, an explanation (NOTE) calmly.
;; M-j t jumps to the next one, then n and p go on, as between flymake's
;; findings.
(use-package
  hl-todo
  :custom
  (hl-todo-keyword-faces
   '(("FIXME" . error)
     ("BUG" . error)
     ("WTF" . error)
     ("WATCHOUT" . warning)
     ("HACK" . warning)
     ("TODO" . font-lock-keyword-face)
     ("NOTE" . success)
     ("INFO" . success)
     ("IDEA" . success)))

  :hook
  (prog-mode . hl-todo-mode)

  :bind
  (:map jtk-jump-map
        ("t" . hl-todo-next))

  :config
  (defvar-keymap jtk-appearance-todo-repeat-map
    :doc "Moving between keywords like TODO and WATCHOUT; the keys repeat."
    :repeat t
    "n" #'hl-todo-next
    "p" #'hl-todo-previous))

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
