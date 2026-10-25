;;; init-sql.el --- SQL clients

;;; Commentary:

;; Options for the MySQL client of sql.el.

;;; Code:

(use-package
  sql
  :ensure nil
  :commands (sql-mysql)
  :config
  (when (equal system-type 'windows-nt)
    (setq sql-mysql-options '("-C" "-t" "-f" "-n"))))

(provide 'init-sql)

;;; init-sql.el ends here
