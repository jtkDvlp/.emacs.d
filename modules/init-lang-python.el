;;; init-lang-python.el --- Python -*- lexical-binding: t; -*-

;;; Commentary:

;; elpy, with keys for structural editing.

;;; Code:

;; pip install jedi rope flake8 autopep8 yapf black
(use-package
  elpy
  :hook (python-mode . elpy-enable)

  :bind
  (:map python-mode-map
        ("M-j d" . elpy-goto-definition)
        ("M-J" . pop-tag-mark))
  
  :config
  (jtk-structural-editing-bind python-mode-map)
  (require 'smartparens-python)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq
   elpy-modules (delq 'elpy-module-flymake elpy-modules)
   elpy-get-info-from-shell t))

(provide 'init-lang-python)

;;; init-lang-python.el ends here
