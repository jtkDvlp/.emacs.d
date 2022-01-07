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
	treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
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
	treemacs-width                         40)

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

(use-package
  simple-bookmarks

  :config
  (defun sql-mysql-for (user password database server &optional buffername)
    (let ((sql-mysql-login-params (when (string-empty-p password) '(password)))
	  (sql-user user)
	  (sql-password password)
	  (sql-database database)
	  (sql-server server))
      (sql-mysql buffername)))

  (defun simple-bookmarks-interactive-add-mysql (&optional name user password database server more)
    (interactive "Smysql-bookmark name: \nsmysql-bookmark user : \nsmysql-bookmark password : \nsmysql-bookmark database : \nsmysql-bookmark server : \ni")
    (simple-bookmarks-interactive-add name 'sql-mysql-for (list user password database server (symbol-name name)) more))

  (defun simple-bookmarks-interactive-execute-mysql ()
    (interactive)
    (simple-bookmarks-interactive-execute (lambda (bookmark) (simple-bookmarks-funcs-type-p 'sql-mysql-for bookmark))))

  (simple-bookmarks-init)

  :bind*
  (("M-- f l" . simple-bookmarks-interactive-execute-file)
   ("M-- f c" . simple-bookmarks-interactive-add-file)
   ("M-- f k" . simple-bookmarks-interactive-remove-file)

   ("M-- d l" . simple-bookmarks-interactive-execute-directory)
   ("M-- d c" . simple-bookmarks-interactive-add-directory)
   ("M-- d k" . simple-bookmarks-interactive-remove-directory)

   ("M-- m l" . simple-bookmarks-interactive-execute-mysql)
   ("M-- m c" . simple-bookmarks-interactive-add-mysql)
   ("M-- m k" . simple-bookmarks-interactive-remove-from-all)))

(custom-set-variables
 '(uniquify-buffer-name-style (quote forward))
 '(uniquify-separator "/")
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-ignore-buffers-re "^\\*")
 '(backup-inhibited t)
 '(delete-by-moving-to-trash nil)
 '(compilation-ask-about-save nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(require-final-newline t))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(global-auto-revert-mode 1)
(custom-set-variables '(global-auto-revert-non-file-buffers t))

(auto-compression-mode t)

(defun zshell ()
  (interactive)
  (ansi-term "/bin/zsh" "zshell"))

(defalias 'zsh 'zshell)

(provide 'base--filemanagement)
