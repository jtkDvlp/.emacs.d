;;; init-lang-typescript.el --- TypeScript -*- lexical-binding: t; -*-

;;; Commentary:

;; tide for TypeScript, and npm commands.

;;; Code:

(use-package
  tide

  :after (typescript-mode company flycheck)

  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)
   (before-save . tide-formater-before-save))

  :config
  (setq tide-completion-detailed t
        tide-always-show-documentation t

        typescript-indent-level 2)

  ;; NOTE: As in JavaScript, moving up or down transposes lines.
  (jtk-structural-editing-bind
   tide-mode-map
   '(("C-M-<up>" . js2-mode-forward-transpose)
     ("C-M-<down>" . js2-mode-backward-transpose)))

  :bind*
  (:map tide-mode-map
        ("M-j d" . tide-jump-to-definition)
        ("M-J" . tide-jump-back)

        ("C-h h" . tide-documentation-at-point)

        ("M-r r" . tide-rename-symbol)
        ("M-r f" . tide-rename-file)

        ("C-C M-j" . tide-restart-server)))

(use-package
  npm-mode

  :commands
  (npm-mode-npm-init
   npm-mode-npm-install
   npm-mode-npm-run))

(provide 'init-lang-typescript)

;;; init-lang-typescript.el ends here
