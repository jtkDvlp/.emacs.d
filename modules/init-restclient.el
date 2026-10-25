;;; init-restclient.el --- HTTP requests

;;; Commentary:

;; restclient for .rest files.

;;; Code:

(use-package
  restclient
  :mode ("\\.rest\\'" . restclient-mode))

(provide 'init-restclient)

;;; init-restclient.el ends here
