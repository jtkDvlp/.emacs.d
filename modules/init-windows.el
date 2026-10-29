;;; init-windows.el --- Windows, frames and tabs -*- lexical-binding: t; -*-

;;; Commentary:

;; Undoing window changes, full-frame views, moving buffers between
;; windows, tabs (as a rule one per project), choosing windows and the
;; IDE-like layout.

;;; Code:

(require 'jtk-project)

(use-package
  winner
  :ensure nil
  :demand t
  :init
  (defun toggle-fullframe-window ()
    (interactive)
    (if (> (length (window-list)) 1)
        (delete-other-windows)
      (winner-undo)))
  :config
  (winner-mode)
  :bind
  (("C-M-#" . toggle-fullframe-window)))

;; NOTE: The point moves along with the buffer.
(use-package
  windmove
  :ensure nil
  :bind
  (:prefix-map jtk-buffer-move-map
   :prefix "C-M-'"
   :prefix-docstring "Moving the buffer to the neighbouring window."
   ("<up>" . windmove-swap-states-up)
   ("<down>" . windmove-swap-states-down)
   ("<left>" . windmove-swap-states-left)
   ("<right>" . windmove-swap-states-right)))

(use-package
  tab-bar
  :ensure nil

  :custom
  ;; NOTE: The bar shows up as soon as there are two tabs.
  (tab-bar-show 1)

  :config
  (defun jtk-tab-new-for-project ()
    "Open a new tab, named after the current project if there is one."
    (interactive)
    (let ((project-name (jtk-project-name)))
      (tab-bar-new-tab)
      (when project-name
        (tab-bar-rename-tab project-name))))

  (defun jtk-tab-close-with-buffers ()
    "Close the current tab and kill the buffers its windows show."
    (interactive)
    (when (length= (tab-bar-tabs) 1)
      (user-error "This is the only tab"))
    (let ((buffers (seq-uniq (mapcar #'window-buffer
                                     (window-list nil 'nomini)))))
      (tab-bar-close-tab)
      (mapc #'kill-buffer buffers)))

  ;; NOTE: The tab commands of Emacs (C-x t), and letters of our own on
  ;; top.  `:prefix' cannot be combined with `:parent', hence `fset'.
  (defvar-keymap jtk-tab-map
    :doc "Tabs: a set of windows each, as a rule one per project."
    :parent tab-prefix-map
    "c" #'jtk-tab-new-for-project
    "C" #'tab-duplicate
    "k" #'tab-close
    "K" #'jtk-tab-close-with-buffers
    "p" #'tab-previous
    "n" #'tab-next
    "N" #'tab-recent
    "t" #'tab-rename
    "f" #'tab-switch)
  (fset 'jtk-tab-map jtk-tab-map)
  (keymap-global-set "M-'" 'jtk-tab-map)

  ;; NOTE: Replaces the repeat map of Emacs for these two, which only
  ;; knows o and O.
  (defvar-keymap jtk-tab-switch-repeat-map
    :doc "Switching tabs; the keys repeat."
    :repeat t
    "n" #'tab-next
    "p" #'tab-previous
    "o" #'tab-next
    "O" #'tab-previous)

  (tab-bar-mode))

(use-package
  ace-window
  :bind
  (("M-#" . ace-window))

  ;; NOTE: Org binds C-# in tables; the global meaning wins.
  :bind*
  (("C-#" . aw-flip-window))

  :config
  (setq
   aw-scope 'frame
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-dispatch-always t))

(defvar-keymap jtk-frame-map
  :doc "Size of the frame; the keys repeat."
  :prefix t
  :repeat t
  "#" #'toggle-frame-maximized)
(keymap-set jtk-view-map "w" 'jtk-frame-map)

(use-package
  jtk-ide
  :ensure nil
  :config
  (jtk-ide-configure-frame-layout jtk-ide-default-layout)

  (defun jtk-ide-setup ()
    (interactive)
    (jtk-ide-open-buffers jtk-ide-default-buffers)))

(provide 'init-windows)

;;; init-windows.el ends here
