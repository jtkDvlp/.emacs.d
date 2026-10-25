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
  :bind*
  (("C-M-#" . toggle-fullframe-window)))

(use-package
  fullframe
  :config
  (setq fullframe/advice-generic-quit-commands nil))

(use-package
  buffer-move
  :config (setq buffer-move-stay-after-swap t)
  :bind
  (("C-M-' <up>" . buf-move-up)
   ("C-M-' <down>" . buf-move-down)
   ("C-M-' <left>" . buf-move-left)
   ("C-M-' <right>" . buf-move-right)))

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

  :bind*
  (("M-' c" . projectile-elscreen-create)
   ("M-' C" . elscreen-clone)

   :map elscreen-map
   ("c" . elscreen-create)
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
  :bind*
  (("M-#" . ace-window)
   ("C-#" . aw-flip-window))

  :config
  (setq
   aw-scope 'frame
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-dispatch-always t))

(use-package
  jtk-window-mirror
  :ensure nil
  :commands window-mirror/start-mirroring)

;; (use-package
;;   google-translate
;;   ())

;; TODO: eigenes package?
(smartrep-define-key
    global-map
    "M-+ w"
  '(("#" . toggle-frame-maximized)))

(use-package
  jtk-ide
  :ensure nil
  :config
  (ide/configure-frame-layout ide/default-layout)

  (defun ide/setup ()
    (interactive)
    (ide/open-buffers ide/default-buffers)))

(provide 'init-windows)
