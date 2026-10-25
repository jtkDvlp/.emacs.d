;;; init-project.el --- Projects -*- lexical-binding: t; -*-

;;; Commentary:

;; projectile with counsel: switching projects and finding files,
;; directories and buffers within one.

;;; Code:

(use-package
  counsel-projectile
  :diminish projectile-mode

  :config
  (setq
   projectile-completion-system 'ivy
   projectile-file-exists-remote-cache-expire 300
   projectile-file-exists-local-cache-expire nil
   projectile-enable-idle-timer t
   projectile-mode-line-prefix " ")
  (counsel-projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories "cache")
  (add-to-list 'projectile-globally-ignored-directories ".cache")

  (defun save-all-buffers ()
    (interactive)
    (save-some-buffers t))

  :bind
  (("C-x b" . counsel-switch-buffer)
   ("C-x B" . counsel-projectile-switch-to-buffer)

   ("C-x f" . counsel-find-file)
   ("C-x F" . counsel-projectile-find-file)

   ("C-x C-f" . counsel-recentf)
   ("C-x C-S-F" . projectile-recentf)

   ("C-x d" . projectile-dired)
   ("C-x D" . counsel-projectile-find-dir)

   ("C-x C-d" . nil)

   ("C-x x f" . counsel-projectile-switch-project)
   ("C-x x F" . counsel-projectile)

   ("C-q" . switch-to-prev-buffer)

   ("C-x C-s" . save-buffer)
   ("C-x C-S-S" . save-all-buffers)

   ("C-x k" . kill-buffer)
   ("C-x K" . projectile-kill-buffers)))

(provide 'init-project)

;;; init-project.el ends here
