(use-package
  simple-bookmarks
  :ensure (:host github :repo "jtkDvlp/simple-bookmarks")

  :init
  ;; NOTE: Unversioned under var/: database bookmarks hold passwords.
  (setq simple-bookmarks-file
        (no-littering-expand-var-file-name "simple-bookmarks.el"))

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

(provide 'init-bookmarks)
