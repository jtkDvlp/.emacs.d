;;; init-startup.el --- Start screen -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard with recent files and projects.

;;; Code:

(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode
  :config
  (setq
   dashboard-startup-banner 3 
   dashboard-items
   '((recents . 20)
     (projects . 10)))
  (dashboard-setup-startup-hook))

(provide 'init-startup)

;;; init-startup.el ends here
