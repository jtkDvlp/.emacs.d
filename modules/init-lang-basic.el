;;; init-lang-basic.el --- BASIC

;;; Commentary:

;; basic-mode for ASP and include files.

;;; Code:

(use-package
  basic-mode
  :ensure nil

  :mode
  (("\\.asp\\'" . basic-mode)
   ("\\.inc\\'" . basic-mode)))

(provide 'init-lang-basic)

;;; init-lang-basic.el ends here
