(use-package
  tide
  :after (typescript-mode company flycheck)

  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode))

  :config
  (setq tide-mode-map (make-sparse-keymap)
	tide-completion-detailed t
	tide-always-show-documentation t)

  (override-keymap* 'sp-keymap (make-keymap* nil sp-keymap))
  
  :bind*
  (:map tide-mode-map
	("M-j d" . tide-jump-to-definition)
	("M-j x" . tide-jump-back)

	("M-h h" . tide-documentation-at-point)

	("M-r r" . tide-rename-symbol)
	("M-r f" . tide-rename-file)

	("C-C M-j" . tide-restart-server)))

(provide 'lang--typescript)
