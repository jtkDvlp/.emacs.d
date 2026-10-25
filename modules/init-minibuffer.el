;;; init-minibuffer.el --- Minibuffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; ivy, counsel and swiper: choosing files, buffers and commands, and
;; searching in a buffer or a git repository.

;;; Code:

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

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
