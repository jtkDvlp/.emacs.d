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

(provide 'base--startup)
