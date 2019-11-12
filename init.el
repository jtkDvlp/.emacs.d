(add-to-list 'load-path "~/.emacs.d/configuration")
(require 'configuration)


(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(setq eshell-prompt-function
      (lambda ()
        (let* ((pwd
                (eshell/pwd))

               (home
                (getenv "HOME"))

               (home-based-path
                (replace-regexp-in-string home "~" pwd))

               (project?
                (projectile-project-root))

               (project-root
                (when (boundp 'projectile-project-root)
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
                (when (boundp 'magit-get-current-branch)
                  (magit-get-current-branch)))

               (git-branch
                (when git?
                  (replace-regexp-in-string (magit-get-current-branch) "master" "m")))

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



(provide 'init)
