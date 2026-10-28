;;; init-bookmarks.el --- Bookmarks -*- lexical-binding: t; -*-

;;; Commentary:

;; simple-bookmarks for files, directories and MySQL connections.  The
;; bookmark file lives under var/: database bookmarks hold passwords.

;;; Code:

(use-package
  simple-bookmarks
  :ensure (:host github :repo "jtkDvlp/simple-bookmarks")

  :init
  ;; NOTE: Unversioned under var/: database bookmarks hold passwords.
  (setq simple-bookmarks-file
        (no-littering-expand-var-file-name "simple-bookmarks.el"))

  :config
  (defun sql-mysql-for (user password database server &optional buffername)
    ;; WATCHOUT: sql has to be loaded before the `let'.  Its variables are
    ;; only special once sql.el has defined them; bound earlier, the
    ;; bindings are lexical and `sql-mysql' never sees them.
    (require 'sql)
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

  :bind
  (:prefix-map jtk-bookmarks-map
   :prefix "M--"
   :prefix-docstring "Bookmarks of files (f), directories (d), MySQL (m)."
   ("f l" . simple-bookmarks-interactive-execute-file)
   ("f c" . simple-bookmarks-interactive-add-file)
   ("f k" . simple-bookmarks-interactive-remove-file)

   ("d l" . simple-bookmarks-interactive-execute-directory)
   ("d c" . simple-bookmarks-interactive-add-directory)
   ("d k" . simple-bookmarks-interactive-remove-directory)

   ("m l" . simple-bookmarks-interactive-execute-mysql)
   ("m c" . simple-bookmarks-interactive-add-mysql)
   ("m k" . simple-bookmarks-interactive-remove-from-all)))

(provide 'init-bookmarks)

;;; init-bookmarks.el ends here
