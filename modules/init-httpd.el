;;; init-httpd.el --- Local web server -*- lexical-binding: t; -*-

;;; Commentary:

;; simple-httpd with commands to serve a directory or the current
;; project, and the URL a served file has.

;;; Code:

(require 'jtk-project)

(defvar httpd-host)
(defvar httpd-port)
(defvar httpd-root)
(declare-function httpd-running-p "simple-httpd")

(defun jtk-httpd--host ()
  "Return the host name the server answers on."
  (pcase httpd-host
    ('nil "0.0.0.0")
    ('local "localhost")
    (_ httpd-host)))

(use-package
  simple-httpd

  :commands
  (jtk-httpd-start
   jtk-httpd-stop
   jtk-httpd-start-project)

  :config
  (defun jtk-httpd-start (root port)
    "Serve the directory ROOT on PORT."
    (interactive "Dhttp-root: \nnhttp-port: ")
    (setq httpd-root root
          httpd-port port)
    (httpd-start)
    (message "Started httpd on %s:%d, serving: %s"
             (jtk-httpd--host) port root))

  (defun jtk-httpd-stop ()
    "Stop the server."
    (interactive)
    (httpd-stop))

  (defun jtk-httpd-start-project ()
    "Serve the current project on port 8080."
    (interactive)
    (jtk-httpd-start (jtk-project-root) 8080)))

(defun jtk-httpd-url (file)
  "Return the URL the running server serves FILE at, or nil."
  (when (and (featurep 'simple-httpd)
             (httpd-running-p)
             (file-in-directory-p file httpd-root))
    (format "http://%s:%d/%s"
            (jtk-httpd--host) httpd-port
            (file-relative-name file httpd-root))))

(provide 'init-httpd)

;;; init-httpd.el ends here
