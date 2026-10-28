;;; init-httpd.el --- Local web server -*- lexical-binding: t; -*-

;;; Commentary:

;; simple-httpd with commands to serve a directory or the current
;; project.

;;; Code:

(use-package
  simple-httpd

  :config
  (defun httpd/start (&optional root port)
    "Starts the server with given `root-directory' at given `port-number'"
    (interactive "Dhttp-root: \nnhttp-port: \n")
    (setf httpd-root root
          httpd-port port)
    (httpd-start)
    (message "Started httpd on %s:%d, serving: %s"
             (cl-case httpd-host
               ((nil) "0.0.0.0")
               ((local) "localhost")
               (otherwise httpd-host))
             port root))

  (defalias 'httpd/stop 'httpd-stop)

  (defun jtk-httpd-start-project ()
    (interactive)
    (httpd/start (projectile-project-root) 8080))

  (defalias 'jtk-httpd-stop-project 'httpd/stop)

  :commands
  (httpd/start
   httpd/stop
   jtk-httpd-start-project
   jtk-httpd-stop-project))

(provide 'init-httpd)

;;; init-httpd.el ends here
