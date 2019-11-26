(use-package eshell
  :ensure nil
  :commands (eshell)

  :config
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

  (setq eshell-login-script "~/eshell_login"
        eshell-rc-script "~/eshellrc")

  (setq eshell-prompt-function
        (lambda ()
          (let* ((home
                  (getenv "HOME"))

                 (pwd
                  (replace-regexp-in-string "~" home (eshell/pwd)))

                 (home-based-path
                  (replace-regexp-in-string home "~" pwd))

                 (project?
                  (projectile-project-root))

                 (project-root
                  (when (fboundp 'projectile-project-root)
                    (projectile-project-root)))

                 (project-name
                  (when project?
                    (funcall projectile-project-name-function project-root)))

                 (project-based-path
                  (when project?
                    (replace-regexp-in-string (substring project-root 0 -1) project-name pwd)))

                 (path-info
                  (if project? project-based-path home-based-path))

                 (git?
                  (when (fboundp 'magit-get-current-branch)
                    (magit-get-current-branch)))

                 (git-branch
                  (when git?
                    (replace-regexp-in-string "master" "m" (magit-get-current-branch))))

                 (git-modified?
                  (and git? (magit-anything-modified-p)))

                 (git-info
                  (if git?
                      (if git-modified?
                          (propertize (format "(%s)" git-branch) 'face 'bold)
                        (format "(%s)" git-branch))
                    "")))

            (format "%s%s \u03bb "
                    path-info git-info)))

        eshell-prompt-regexp
        "^[^\u03bb\n]*\u03bb\s")

  (add-hook
   'eshell-mode-hook
   '(lambda ()
      (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point))))

(use-package eshell-up
  :commands
  (eshell-up eshell-up-peek))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(provide 'base--shell)
