(use-package
  c++-mode
  :ensure nil

  :mode
  (("\\.ino\\'" . c++-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode))

  :hook
  ((c++-mode . (lambda () (rainbow-mode 0)))))

(use-package
  platformio-mode

  :mode
  (("platformio\\.ini\\'" . platformio-mode))

  :hook
  ((c++-mode . platformio-mode)))

(provide 'lang--c)
