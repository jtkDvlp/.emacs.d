;;; init-snippets.el --- Snippets -*- lexical-binding: t; -*-

;;; Commentary:

;; yasnippet with the yasnippet-snippets collection; own snippets are
;; under etc/yasnippet/snippets/.

;;; Code:

(use-package
  yasnippet-snippets
  :defer t)

(use-package
  yasnippet
  :diminish yas-minor-mode
  :hook
  ((prog-mode . yas-minor-mode-on)
   (cider-repl-mode . yas-minor-mode-on))

  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)

  :bind*
  (:map yas-minor-mode-map
        ("C-c" . nil)))

(provide 'init-snippets)

;;; init-snippets.el ends here
