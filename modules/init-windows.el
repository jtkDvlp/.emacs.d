;;; init-windows.el --- Windows, frames and tabs -*- lexical-binding: t; -*-

;;; Commentary:

;; Undoing window changes, a single window, moving buffers between
;; windows, tabs (as a rule one per project), choosing windows and the
;; IDE-like layout.

;;; Code:

(require 'jtk-project)

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

;; NOTE: The window history is kept per tab; winner keeps one per frame
;; and so mixes up the tabs.  C-c <left> and C-c <right>, the arrows
;; repeat.
(tab-bar-history-mode)

(defvar jtk-window--before-single (make-hash-table :test #'eq :weakness 'key)
  "Window configurations before `jtk-window-toggle-single', by window.")

(defun jtk-window--main-windows ()
  "Return the windows of the frame, side windows left out."
  (seq-remove (lambda (window) (window-parameter window 'window-side))
              (window-list nil 'nomini)))

;; NOTE: The configuration is kept by itself rather than going back in
;; the tab's history: that history leaves out a command repeated in a
;; row, so every other C-M-# would go back too far.
(defun jtk-window-toggle-single ()
  "Show only the selected window, or bring the other windows back."
  (interactive)
  (let ((window (selected-window)))
    (cond ((length> (jtk-window--main-windows) 1)
           (puthash window (current-window-configuration)
                    jtk-window--before-single)
           (delete-other-windows))
          ((gethash window jtk-window--before-single)
           (set-window-configuration
            (gethash window jtk-window--before-single))
           (remhash window jtk-window--before-single))
          (t
           (user-error "No other windows to bring back")))))
(keymap-global-set "C-M-#" #'jtk-window-toggle-single)

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
