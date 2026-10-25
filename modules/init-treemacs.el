;;; init-treemacs.el --- File tree side bar -*- lexical-binding: t; -*-

;;; Commentary:

;; treemacs and its integrations with projectile, dired and magit.

;;; Code:

(use-package
  treemacs
  :config
  (defun treemacs-hide ()
    (interactive)
    (delete-window (treemacs-get-local-window)))

  (defun treemacs-select-current-project-file ()
    (interactive)
    (save-excursion
      (treemacs-add-and-display-current-project))
    (treemacs-find-file)
    (treemacs-select-window))

  (defun treemacs-select-current-file ()
    (interactive)
    (let ((path (buffer-file-name (current-buffer))))
      (when path
        (let ((project (treemacs--find-project-for-path path)))
          (unless project
            (save-excursion
              (call-interactively 'treemacs-add-project-to-workspace))))
        (treemacs-select-window)
        (treemacs-goto-file-node path project))))

  (defun treemacs-dwim ()
    (interactive)
    (if (eq (get-buffer-window) (treemacs-get-local-window))
        (treemacs-hide)
      (if (treemacs--find-current-user-project)
          (treemacs-select-current-project-file)
        (treemacs-select-current-file))))

  (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             nil
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         t
        treemacs-max-git-entries               5000
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       nil
        treemacs-project-follow-cleanup        nil
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-asc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         40
        treemacs-width-is-initially-locked     nil)

  :bind*
  (("C-x x d" . treemacs-dwim)
   ("C-x x D" . treemacs-hide)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(provide 'init-treemacs)

;;; init-treemacs.el ends here
