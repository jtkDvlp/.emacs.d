;;; init-vc.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:

;; magit, including pushes for review to Gerrit, and
;; git-auto-commit-mode.

;;; Code:

(use-package
  magit
  :requires fullframe
  :bind*
  (("C-v" . nil)
   ("M-v g" . magit-status)
   ("M-v l" . magit-log-current)
   ("M-v L" . magit-log-all)
   ("M-v b" . magit-blame)
   ("M-v B" . magit-log-buffer-file))

  :init
  (defun magit-push-to-gerrit-local-branch-or-commit-to-branch (source target)
    (interactive
     (let ((source (magit-read-local-branch-or-commit "Push"))
           (target (magit-read-local-branch-or-commit "On")))
       (list source target)))
    (magit-git-command-topdir (concat "git push origin " source (concat ":refs/for/" target))))

  (defun magit-push-to-gerrit-local-branch-or-commit (source)
    (interactive
     (let ((source (magit-read-local-branch-or-commit "Push")))
       (list source)))
    (magit-git-command-topdir (concat "git push origin " source (concat ":refs/for/" source))))

  (defun magit-push-to-gerrit-current-branch ()
    (interactive)
    (magit-push-to-gerrit-local-branch-or-commit (magit-get-current-branch)))

  :config
  (transient-append-suffix 'magit-push "e"
    '("P" "Push current branch to Gerrit" magit-push-to-gerrit-current-branch))
  (transient-append-suffix 'magit-push "e"
    '("U" "Push branch / commit to Gerrit" magit-push-to-gerrit-local-branch-or-commit))
  (transient-append-suffix 'magit-push "e"
    '("A" "Push branch / commit to Gerrit on branch" magit-push-to-gerrit-local-branch-or-commit-to-branch))
  (fullframe magit-status magit-mode-quit-window)
  (fullframe magit-log-all magit-mode-quit-window)
  (fullframe magit-log-current magit-mode-quit-window))

(use-package
  git-auto-commit-mode
  :diminish git-auto-commit-mode

  :config
  (setq-default
   gac-ask-for-summary-p nil
   gac-debounce-interval (* 10 60)))

(provide 'init-vc)

;;; init-vc.el ends here
