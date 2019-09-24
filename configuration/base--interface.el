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
  (ivy-mode 1))

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

  :bind
  (("C-s" . swiper)
   ("C-S-s" . counsel-git-grep)

   ("C-r" . swiper)
   ("C-S-r" . counsel-git-grep-query-replace)
   
   :map swiper-map
   ("C-r" . swiper-query-replace)))

(use-package
  fullframe
  :commands fullframe)

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
  (elscreen-create)
  
  :bind*
  (("M-' c" . elscreen-create)
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
  (defun elscreen (open-command close-command)
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
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (elscreen #'ediff-buffers #'ediff-quit)
  (elscreen #'ediff-files #'ediff-quit)
  (elscreen #'ediff #'ediff-quit)  
  
  :commands
  (ediff-buffers
   ediff-files
   ediff))

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
   ("<return>" . nil)))

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
  (setq default-input-method "MacOSX"
        ns-right-alternate-modifier nil))

(provide 'base--interface)
