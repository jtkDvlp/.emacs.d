(use-package
  scss-mode

  :mode
  "\\.\\(scss\\)\\'"

  :config
  (setq scss-compile-at-save nil))

(use-package
  sass-mode

  :mode
  "\\.\\(sass\\)\\'"

  :config
  (setq scss-compile-at-save nil))

(use-package
  css-mode

  :mode
  "\\.\\(css\\)\\'"

  :config
  (setq css-indent-offset 2))

(provide 'lang--css)
