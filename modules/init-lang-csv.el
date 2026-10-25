;;; init-lang-csv.el --- CSV files

;;; Commentary:

;; csv-mode with aligned columns.

;;; Code:

(use-package
  csv-mode

  :mode
  ("\\.csv\\'" . enable-csv-mode)

  :init
  (defun enable-csv-mode ()
    (csv-mode)
    (csv-align-mode))

  :config
  (setq csv-separators '(";" ",")
        csv-align-style 'auto))

(provide 'init-lang-csv)

;;; init-lang-csv.el ends here
