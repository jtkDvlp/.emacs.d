;;; init-lang-css.el --- Style sheets

;;; Commentary:

;; CSS, SCSS and Sass.

;;; Code:

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
  :ensure nil

  :mode
  "\\.\\(css\\)\\'"

  :config
  (setq css-indent-offset 2))

(provide 'init-lang-css)

;;; init-lang-css.el ends here
