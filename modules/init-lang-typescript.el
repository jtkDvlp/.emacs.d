;;; init-lang-typescript.el --- TypeScript -*- lexical-binding: t; -*-

;;; Commentary:

;; tide for TypeScript, and npm commands.

;;; Code:

(require 'jtk-project)

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

  :bind
  (:map tide-mode-map
        ("M-j d" . tide-jump-to-definition)

        ("C-h h" . tide-documentation-at-point)

        ("M-r r" . tide-rename-symbol)
        ("M-r f" . tide-rename-file)

        ("C-c M-j" . tide-restart-server)))

(use-package
  npm-mode

  :commands
  (npm-mode-npm-init
   npm-mode-npm-install
   npm-mode-npm-run)

  :init
  ;; NOTE: npm-mode's own keys (install i, run r, ...), under C-c M-j in
  ;; JavaScript buffers and under j in the project commands.
  (add-to-list 'jtk-project-kinds
               '("npm" ("package.json") npm-mode-command-keymap npm-mode)
               t)
  (with-eval-after-load 'js2-mode
    (require 'npm-mode)
    (keymap-set js2-mode-map "C-c M-j" npm-mode-command-keymap)))

(provide 'init-lang-typescript)

;;; init-lang-typescript.el ends here
