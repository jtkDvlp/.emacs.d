(use-package
  c++-mode
  :ensure nil

  :mode
  (("\\.ino\\'" . c++-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode)))

(use-package
  platformio-mode
  :hook (c++-mode . platformio-mode))

(provide 'lang--c)
