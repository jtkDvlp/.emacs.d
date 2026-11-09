;;; init-startup.el --- Start page -*- lexical-binding: t; -*-

;;; Commentary:

;; The dashboard as start page: the Clojure logo, then recent projects,
;; recent files and bookmarks with nerd-icons, centered in the window.
;; p, r and m jump to the lists; RET opens an entry.

;;; Code:

(declare-function no-littering-expand-var-file-name "no-littering")
(defvar dashboard-startup-banner)

;; NOTE: The Clojure logo is not in the repository, which is public and
;; the logo not ours to hand on; the first start in the GUI fetches it
;; into var/.  Without it, the page shows Emacs' logo.
(defconst jtk-startup-logo-url
  "https://clojure.org/images/clojure-logo-120b.png"
  "Where the logo of the start page comes from.")

(defun jtk-startup-logo ()
  "Return the banner of the start page: the Clojure logo, or Emacs' logo.
The Clojure logo is fetched once from `jtk-startup-logo-url'."
  (let ((file (no-littering-expand-var-file-name "dashboard/clojure-logo.png")))
    (unless (or (file-exists-p file) (not (display-graphic-p)))
      (make-directory (file-name-directory file) t)
      (condition-case err
          (let ((part (concat file ".part")))
            (url-copy-file jtk-startup-logo-url part t)
            (rename-file part file t))
        (error (message "Clojure logo not fetched: %s"
                        (error-message-string err)))))
    (if (file-exists-p file) file 'logo)))

(use-package
  dashboard
  :demand t
  :diminish page-break-lines-mode

  :custom
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
  (setq dashboard-startup-banner (jtk-startup-logo))
  ;; NOTE: elpaca configures packages after init, so the dashboard is
  ;; filled and shown from `elpaca-after-init-hook' (dashboard's README).
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))

(provide 'init-startup)

;;; init-startup.el ends here
