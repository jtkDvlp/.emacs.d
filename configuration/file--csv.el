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

(provide 'file--csv)
