;;; init-windows.el --- Windows, frames and screens -*- lexical-binding: t; -*-

;;; Commentary:

;; Undoing window changes, full-frame views, moving buffers between
;; windows, screens per project (elscreen), choosing windows and the
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

(use-package
  fullframe
  :config
  (setq fullframe/advice-generic-quit-commands nil))

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
  elscreen

  :commands
  (elscreen-create jtk-elscreen-create-for-project)

  :init
  (defun jtk-elscreen-create-for-project ()
    "Create a screen named after the current project."
    (interactive)
    (let ((project-name (jtk-project-name)))
      (elscreen-create)
      (when project-name
        (elscreen-screen-nickname project-name))
      nil))

  ;; NOTE: M-' loads elscreen on first use; elscreen then puts its map
  ;; there itself.
  :bind-keymap
  ("M-'" . elscreen-map)

  :bind
  (:map elscreen-map
   ("c" . jtk-elscreen-create-for-project)
   ("C" . elscreen-clone)
   ("k" . elscreen-kill)
   ("K" . elscreen-kill-screen-and-buffers)
   ("p" . elscreen-previous)
   ("n" . elscreen-next)
   ("N" . elscreen-toggle)
   ("t" . elscreen-screen-nickname)
   ("f" . elscreen-select-and-goto))

  :init
  (defun with-elscreen (open-command close-command)
    (advice-add
     open-command
     :before
     (lambda (&rest args)
       (call-interactively 'elscreen-create))
     '((name . "elscreen-open-command")))

    (advice-add
     close-command
     :after
     (lambda (&rest args)
       (call-interactively 'elscreen-kill))
     '((name . "elscreen-close-command"))))

  :config
  (setq
   elscreen-prefix-key "\M-'"
   elscreen-display-screen-number nil
   elscreen-display-tab nil)

  (elscreen-start))

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
