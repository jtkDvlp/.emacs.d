;;; init-vc.el --- Version control -*- lexical-binding: t; -*-

;;; Commentary:

;; magit, including pushes for review to Gerrit.

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

  (defun jtk-vc-gerrit-push-to-branch (source target)
    "Push SOURCE, a branch or commit, to Gerrit for review on TARGET."
    (interactive
     (list (magit-read-local-branch-or-commit "Push")
           (magit-read-local-branch-or-commit "On")))
    (magit-git-command-topdir
     (format "git push origin %s:refs/for/%s" source target)))

  (defun jtk-vc-gerrit-push (source)
    "Push SOURCE, a branch or commit, to Gerrit for review on itself."
    (interactive
     (list (magit-read-local-branch-or-commit "Push")))
    (jtk-vc-gerrit-push-to-branch source source))

  (defun jtk-vc-gerrit-push-current-branch ()
    "Push the current branch to Gerrit for review."
    (interactive)
    (jtk-vc-gerrit-push (magit-get-current-branch)))

  :config
  (transient-append-suffix 'magit-push "e"
    '("P" "Push current branch to Gerrit" jtk-vc-gerrit-push-current-branch))
  (transient-append-suffix 'magit-push "e"
    '("U" "Push branch / commit to Gerrit" jtk-vc-gerrit-push))
  (transient-append-suffix 'magit-push "e"
    '("A" "Push branch / commit to Gerrit on branch"
      jtk-vc-gerrit-push-to-branch)))

(provide 'init-vc)

;;; init-vc.el ends here
