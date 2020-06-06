(use-package
  scss-mode

  :mode
  "\\.\\(scss\\)\\'")

(use-package
  sass-mode

  :mode
  "\\.\\(sass\\)\\'"

  :config
  (setq scss-compile-at-save nil))

(provide 'lang--css)
