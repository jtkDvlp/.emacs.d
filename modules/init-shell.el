;;; init-shell.el --- Shells -*- lexical-binding: t; -*-

;;; Commentary:

;; eshell with a project-aware prompt, history and commands; aliases
;; are in etc/eshell/aliases.  Also zsh in a terminal.

;;; Code:

(require 'jtk-project)

(use-package eshell
  :ensure nil
  :commands (eshell)

  :init
  (defvar eshell-mode-map* (make-sparse-keymap))

  :bind
  (:map jtk-run-map
        :prefix-map jtk-shell-map
        :prefix "s"
        :prefix-docstring "Shells: eshell, and a single shell command."
        ("s" . eshell-dwim)
        ("S" . eshell)
        ("c" . shell-command)

   :map eshell-mode-map*
   ("<up>" . previous-line)
   ("<down>" . next-line)
   ("C-<up>" . eshell-previous-prompt)
   ("C-<down>" . eshell-next-prompt)

   ("C-<return>" . end-of-buffer)

   ("C-o" . eshell-insert-buffer-name)

   ("M-p" . eshell-previous-matching-input-from-input)
   ("M-n" . eshell-next-matching-input-from-input)

   ("M-P" . consult-history))

  :config
  (require 'em-smart)

  (defun eshell-dwim (arg)
    (interactive "P")
    (if (jtk-project-root)
        (call-interactively #'project-eshell)
      (eshell arg)))

  ;; NOTE: eshell used to create its mode map only when the mode starts,
  ;; so bindings are collected in `eshell-mode-map*' and copied over then.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (replace-keymap-bindings eshell-mode-map* eshell-mode-map)))

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-hist-ignoredups t
        eshell-history-size 1024)

  (setq eshell-rc-script "~/.eshellrc")

  (setq eshell-prompt-function
        (lambda ()
          (let* ((home
                  (getenv "HOME"))

                 (pwd
                  (replace-regexp-in-string "~" home (concat (eshell/pwd) "/")))

                 (remote?
                  (string-match tramp-file-name-regexp pwd))

                 (local?
                  (not remote?))

                 (home-based-path
                  (replace-regexp-in-string home "~" pwd))

                 (project?
                  (when local?
                    (jtk-project-root)))

                 (project-root
                  project?)

                 (project-name
                  (when project?
                    (jtk-project-name)))

                 (project-based-path
                  (when project?
                    (replace-regexp-in-string project-root ":/" pwd)))

                 (git?
                  (when (and project? (fboundp 'magit-get-current-branch))
                    (magit-get-current-branch)))

                 (git-branch
                  (when git?
                    (replace-regexp-in-string "master" "m" (magit-get-current-branch))))

                 (git-modified?
                  (and git? (magit-anything-modified-p)))

                 (git-info
                  (if git?
                      (if git-modified?
                          (propertize (format "(%s)" git-branch) 'face 'bold)
                        (format "(%s)" git-branch))
                    ""))

                 (project-info
                  (if project?
                      (format "%s%s\n%s" project-name git-info project-based-path)
                    ""))

                 (info
                  (if project?
                      project-info
                    home-based-path)))

            (format "\n%s\n\u21b3 \u03bb " info)))

        eshell-prompt-regexp
        "^[^\u03bb]*\u03bb\s")

  (defun eshell/repl (&rest profile)
    (if (seq-empty-p profile)
        (call-interactively 'jtk-clojure-jack-in-dwim)
      (jtk-clojure-jack-in-with-profile (apply 'concat profile))))

  (defun eshell/nvm (version)
    (nvm-use version))

  (defun eshell/open (file)
    (browse-url-of-file file))

  (add-hook
   'eshell-mode-hook
   '(lambda ()
      (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point))))

(use-package eshell-up
  :commands
  (eshell-up eshell-up-peek))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(defun zshell ()
  (interactive)
  (ansi-term "/bin/zsh" "zshell"))

(defalias 'zsh 'zshell)

(defun replace-keymap-bindings (from to)
  (map-keymap
   (lambda (k f)
     (define-key to (vector k) f))
   from))

(provide 'init-shell)

;;; init-shell.el ends here
