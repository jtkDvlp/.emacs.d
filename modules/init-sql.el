;;; init-sql.el --- SQL clients -*- lexical-binding: t; -*-

;;; Commentary:

;; sql.el's MySQL client; saved connections are bookmarks
;; (init-bookmarks).

;;; Code:

(use-package
  sql
  :ensure nil
  :commands (sql-mysql))

(provide 'init-sql)

;;; init-sql.el ends here
