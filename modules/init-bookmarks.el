;;; init-bookmarks.el --- Bookmarks -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs' own bookmarks for files and directories, chosen with consult,
;; and bookmarks of MySQL connections, whose passwords come from
;; auth-source (~/.authinfo.gpg) instead of the bookmark file.  The
;; bookmark file lives under var/.  Bookmarks of simple-bookmarks, used
;; before, can be imported once.

;;; Code:

(require 'auth-source)
(require 'bookmark)

(declare-function sql-mysql "sql")
(defvar sql-user)
(defvar sql-database)
(defvar sql-server)
(defvar sql-password)
(defvar sql-mysql-login-params)

(defun jtk-bookmarks--mysql-record (user database server)
  "Return a bookmark record for USER's connection to DATABASE on SERVER."
  `((filename . ,(format "mysql://%s@%s/%s" user server database))
    (handler . jtk-bookmarks-mysql-jump)
    (sql-user . ,user)
    (sql-database . ,database)
    (sql-server . ,server)))

(defun jtk-bookmarks-set-mysql (name user database server)
  "Bookmark USER's MySQL connection to DATABASE on SERVER as NAME.
The password is looked up in auth-source when connecting, by SERVER and
USER, or asked for."
  (interactive
   (list (read-string "Bookmark name: ")
         (read-string "User: ")
         (read-string "Database: ")
         (read-string "Server: " "localhost")))
  (bookmark-store name (jtk-bookmarks--mysql-record user database server)
                  nil)
  (bookmark-save))

(defun jtk-bookmarks-mysql-jump (bookmark)
  "Connect to the MySQL database of BOOKMARK."
  ;; WATCHOUT: sql has to be loaded before the `let'.  Its variables are
  ;; only special once sql.el has defined them; bound earlier, the
  ;; bindings are lexical and `sql-mysql' never sees them.
  (require 'sql)
  (let* ((user (bookmark-prop-get bookmark 'sql-user))
         (server (bookmark-prop-get bookmark 'sql-server))
         (password (auth-source-pick-first-password :host server :user user))
         (sql-user user)
         (sql-server server)
         (sql-database (bookmark-prop-get bookmark 'sql-database))
         (sql-password (or password ""))
         (sql-mysql-login-params (unless password '(password))))
    (set-buffer (sql-mysql (bookmark-name-from-full-record bookmark)))))

(defun jtk-bookmarks--import-one (entry)
  "Store the simple-bookmarks ENTRY as a bookmark.
Return an auth-source line for its password, or nil."
  (let ((name (format "%s" (car (alist-get 'name entry))))
        (func (car (alist-get 'func entry)))
        (args (car (alist-get 'args entry))))
    (pcase func
      ((or 'find-file 'dired)
       (bookmark-store name `((filename . ,(expand-file-name (car args))))
                       nil)
       nil)
      ('sql-mysql-for
       (pcase-let ((`(,user ,password ,database ,server . ,_) args))
         (bookmark-store name
                         (jtk-bookmarks--mysql-record user database server)
                         nil)
         (unless (string-empty-p password)
           (format "machine %s login %s password %s" server user password))))
      (_ (message "Not imported: %s (%s)" name func) nil))))

(defun jtk-bookmarks-import-simple-bookmarks (file)
  "Import the bookmarks of simple-bookmarks from FILE.
Passwords of MySQL bookmarks are not stored; a buffer lists them as
lines for ~/.authinfo.gpg."
  (interactive
   (list (read-file-name "simple-bookmarks file: " nil
                         (no-littering-expand-var-file-name
                          "simple-bookmarks.el")
                         t)))
  (let* ((entries (with-temp-buffer
                    (insert-file-contents file)
                    (read (current-buffer))))
         (secrets (delq nil (mapcar #'jtk-bookmarks--import-one entries))))
    (bookmark-save)
    (message "Read %d simple-bookmarks entries" (length entries))
    (when secrets
      (with-current-buffer (get-buffer-create "*authinfo lines*")
        (erase-buffer)
        (insert ";; Add these lines to ~/.authinfo.gpg, then kill this "
                "buffer.\n"
                (string-join secrets "\n") "\n")
        (pop-to-buffer (current-buffer))))))

(use-package
  bookmark
  :ensure nil

  :custom
  ;; NOTE: Saved with every change, not only when Emacs exits.
  (bookmark-save-flag 1)

  :bind
  (:prefix-map jtk-bookmarks-map
   :prefix "M--"
   :prefix-docstring
   "Bookmarks: jump (l), set (c), MySQL (m), delete (k), list (e)."
   ("l" . consult-bookmark)
   ("c" . bookmark-set)
   ("m" . jtk-bookmarks-set-mysql)
   ("k" . bookmark-delete)
   ("e" . bookmark-bmenu-list)))

(provide 'init-bookmarks)

;;; init-bookmarks.el ends here
