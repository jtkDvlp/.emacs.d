;;; init-project.el --- Projects -*- lexical-binding: t; -*-

;;; Commentary:

;; projectile: switching projects and finding files,
;; directories and buffers within one.

;;; Code:

(use-package
  projectile
  :diminish projectile-mode

  :config
  (setq
   projectile-completion-system 'default
   projectile-file-exists-remote-cache-expire 300
   projectile-file-exists-local-cache-expire nil
   projectile-enable-idle-timer t
   projectile-mode-line-prefix " ")
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-to-list 'projectile-globally-ignored-directories "cache")
  (add-to-list 'projectile-globally-ignored-directories ".cache")

  (defun save-all-buffers ()
    (interactive)
    (save-some-buffers t))

  :bind
  (("C-x b" . consult-buffer)
   ("C-x B" . consult-project-buffer)

   ("C-x f" . find-file)
   ("C-x F" . projectile-find-file)

   ("C-x C-f" . consult-recent-file)
   ("C-x C-S-F" . projectile-recentf)

   ("C-x d" . projectile-dired)
   ("C-x D" . projectile-find-dir)

   ("C-x C-d" . nil)

   ("C-x x f" . projectile-switch-project)
   ("C-x x F" . consult-project-buffer)

   ("C-q" . switch-to-prev-buffer)

   ("C-x C-s" . save-buffer)
   ("C-x C-S-S" . save-all-buffers)

   ("C-x k" . kill-buffer)
   ("C-x K" . projectile-kill-buffers)))

(provide 'init-project)

;;; init-project.el ends here
