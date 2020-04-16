(use-package
  basic-mode
  :ensure nil

  :mode
  (("\\.asp\\'" . basic-mode)
   ("\\.inc\\'" . basic-mode)))

(provide 'lang--basic)
