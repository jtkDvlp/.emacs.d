;;; init-windows.el --- Windows, frames and screens -*- lexical-binding: t; -*-

;;; Commentary:

;; Undoing window changes, full-frame views, moving buffers between
;; windows, screens per project (elscreen), choosing windows,
;; mirroring a window and the IDE-like layout.

;;; Code:

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

(use-package
  buffer-move
  :config (setq buffer-move-stay-after-swap t)
  :bind
  (:prefix-map jtk-buffer-move-map
   :prefix "C-M-'"
   :prefix-docstring "Moving the buffer to the neighbouring window."
   ("<up>" . buf-move-up)
   ("<down>" . buf-move-down)
   ("<left>" . buf-move-left)
   ("<right>" . buf-move-right)))

(use-package
  elscreen

  :commands
  (elscreen-create projectile-elscreen-create)

  :init
  (defun projectile-elscreen-create ()
    (interactive)
    (let* ((project-root
            (when (fboundp 'projectile-project-root)
              (projectile-project-root)))

           (project-name
            (when project-root
              (funcall projectile-project-name-function project-root))))

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
   ("c" . projectile-elscreen-create)
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

(use-package
  jtk-window-mirror
  :ensure nil
  :commands jtk-window-mirror-start-mirroring)

;; (use-package
;;   google-translate
;;   ())

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
