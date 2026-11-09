;;; init-startup.el --- Start page -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard as start page: the Emacs logo, then recent projects,
;; recent files and bookmarks with nerd-icons, centered in the window.
;; p, r and m jump to the lists; RET opens an entry.

;;; Code:

(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode

  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-banner-logo-title nil)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((projects . 8) (recents . 8) (bookmarks . 5)))
  (dashboard-navigation-cycle t)

  :config
  ;; NOTE: elpaca configures packages after init, so the dashboard is
  ;; filled and shown from `elpaca-after-init-hook' (dashboard's README).
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(provide 'init-startup)

;;; init-startup.el ends here
