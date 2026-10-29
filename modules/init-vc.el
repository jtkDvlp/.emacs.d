;;; init-vc.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:

;; magit, including pushes for review to Gerrit, and
;; git-auto-commit-mode.

;;; Code:

(use-package
  magit
  :custom
  (magit-display-buffer-function #'jtk-vc-display-buffer)
  ;; NOTE: q brings back the windows as they were before the buffer
  ;; showed up, the full-frame status and logs included.
  (magit-bury-buffer-function #'magit-restore-window-configuration)

  :bind
  (:prefix-map jtk-vc-map
   :prefix "M-v"
   :prefix-docstring "Git: status, logs and blame."
   ("g" . magit-status)
   ("l" . magit-log-current)
   ("L" . magit-log-all)
   ("b" . magit-blame)
   ("B" . magit-log-buffer-file))

  :init
  (defun jtk-vc-display-buffer (buffer)
    "Display the magit BUFFER; status and logs fill the frame."
    (if (with-current-buffer buffer
          (derived-mode-p 'magit-status-mode 'magit-log-mode))
        (let ((window (display-buffer buffer
                                      '((display-buffer-reuse-window
                                         display-buffer-same-window
                                         display-buffer-pop-up-window)))))
          (delete-other-windows window)
          window)
      (magit-display-buffer-traditional buffer)))

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
    '("A" "Push branch / commit to Gerrit on branch" magit-push-to-gerrit-local-branch-or-commit-to-branch)))

(use-package
  git-auto-commit-mode
  :diminish git-auto-commit-mode

  :config
  (setq-default
   gac-ask-for-summary-p nil
   gac-debounce-interval (* 10 60)))

(provide 'init-vc)

;;; init-vc.el ends here
