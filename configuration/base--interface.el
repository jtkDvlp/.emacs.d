(use-package
  kmacro
  :ensure nil

  :bind*
  (("M-! m s" . start-kbd-macro)
   ("M-! m S" . kmacro-end-macro)
   ("M-! m m" . kmacro-end-and-call-macro)))

(use-package
  ivy
  :demand t
  :diminish ivy-mode

  :config
  (setq
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-height 15
   ivy-wrap t
   ivy-use-virtual-buffers t)
  (ivy-mode 1)

  :bind*
  (:map ivy-minibuffer-map
        ("C-o" . ivy-occur)))

(use-package
  counsel
  :demand t
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (delete-selection-mode t)
  :bind*
  (("C-S-Y" . counsel-yank-pop)))

(use-package
  swiper
  :requires counsel

  :config
  (defun counsel-git-grep-dwim ()
    (interactive)
    (let ((text (when mark-active (buffer-substring-no-properties (region-beginning) (region-end)))))
      (deactivate-mark)
      (counsel-git-grep text)))

  :bind
  (("C-s" . swiper)
   ("C-S-s" . counsel-git-grep-dwim)

   ("C-r" . swiper)
   ("C-S-r" . counsel-git-grep-query-replace)

   :map swiper-map
   ("C-r" . swiper-query-replace)))

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
              (projectile-project-name-function project-root))))

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
  ediff
  :ensure nil

  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)
  (with-elscreen #'ediff-buffers #'ediff-quit)
  (with-elscreen #'ediff-files #'ediff-quit)
  (with-elscreen #'ediff-directories #'ediff-quit)

  :commands
  (ediff-buffers
   ediff-files
   ediff))

(use-package
  ztree
  :demand t

  :commands
  (ztree-diff)

  :init
  (defalias 'ediff-trees 'ztree-diff)

  (setq ztree-diff-filter-list '("^\\.$" "^\\.\\.$" "^\\.git$" "^\\.DS_Store$" "^node_modules$")
        ztree-draw-unicode-lines t))

(use-package
  ace-jump-mode
  :bind
  (("C-j" . ace-jump-char-mode)
   ("C-S-j" . ace-jump-mode-pop-mark)
   ("M-j" . nil)
   ("M-j l" . ace-jump-line-mode)))

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
  expand-region
  :bind*
  (("C-." . er/expand-region)
   ("C-:" . er/contract-region)))

(use-package
  pkg--narrow
  :load-path "configuration/"
  :commands narrow/narrow-dwim
  :bind*
  (("C--" . narrow/narrow-dwim)
   ("C-+" . narrow/widen)))

(use-package
  which-key
  :demand t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

(use-package
  undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package
  multiple-cursors

  :config
  (define-key mc/keymap (kbd "<return>") nil)

  :bind*
  (("M-c m" . mc/mark-more-like-this-extended)
   ("M-c x" . mc/mark-all-like-this-dwim)
   ("M-c a" . mc/mark-all-like-this)
   ("M-c l" . mc/edit-lines)

   ("M-c i n" . mc/insert-numbers)
   ("M-c i l" . mc/insert-letters)
   ("M-c i s" . mc/sort-region)
   ("M-c i r" . mc/reverse-region)

   :map mc/keymap
   ("<tab>" . mc-hide-unmatched-lines-mode)))

(use-package
  pkg--zoom
  :demand t
  :load-path "configuration/"
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

(use-package
  pkg--window-mirror
  :load-path "configuration/"
  :commands window-mirror/start-mirroring)

;; (use-package
;;   google-translate
;;   ())

(custom-set-variables
 '(transient-mark-mode t)
 '(highlight-nonselected-windows t)
 '(shift-select-mode t)
 '(x-select-enable-clipboard t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-buffer-choice "*empty*")
 '(ring-bell-function (quote ignore)))

(defalias 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'darwin)
  (setq
   ns-alternate-modifier nil
   ns-right-alternate-modifier nil
   mac-command-modifier 'meta))

(provide 'base--interface)
