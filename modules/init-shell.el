;;; init-shell.el --- Shells -*- lexical-binding: t; -*-

;;; Commentary:

;; eshell with a project-aware prompt, history and commands, and a
;; suggestion from the history while typing; aliases are in
;; etc/eshell/aliases.  eat is the terminal: zsh on its own, and the
;; display of programs in eshell that need a terminal (top, less, ...).
;; mistty runs zsh too, but edits its command line with Emacs' keys.
;; In eshell, bbe and clje evaluate Clojure with a local Babashka, also
;; on data from a remote host (jtk-babashka).

;;; Code:

(require 'jtk-project)
(require 'jtk-babashka)

(defvar corfu-auto)
(declare-function eshell/pwd "em-dirs")
(declare-function magit-anything-modified-p "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function eat "eat")
(declare-function mistty "mistty")
(declare-function mistty-in-project "mistty-project")

(defun jtk-shell-eshell (arg)
  "Open eshell in the current project's root, or as `eshell' outside one.
ARG is passed on as to `eshell'."
  (interactive "P")
  (if (jtk-project-root)
      (call-interactively #'project-eshell)
    (eshell arg)))

;; NOTE: Runs git twice through magit (branch, anything modified) for
;; every prompt; 6.7 ms in this repository (Emacs 31.1, cloud, batch).
(defun jtk-shell-prompt ()
  "Return the eshell prompt: project, branch and path, or the path."
  (let* ((home (getenv "HOME"))
         (pwd (replace-regexp-in-string
               "~" home (concat (eshell/pwd) "/")))
         (remote? (string-match tramp-file-name-regexp pwd))
         (project-root (unless remote? (jtk-project-root)))
         (git-branch (when-let* (((and project-root
                                       (fboundp 'magit-get-current-branch)))
                                 (branch (magit-get-current-branch)))
                       (replace-regexp-in-string "master" "m" branch)))
         (git-info
          (cond ((null git-branch) "")
                ((magit-anything-modified-p)
                 (propertize (format "(%s)" git-branch) 'face 'bold))
                (t (format "(%s)" git-branch))))
         (info
          (if project-root
              (format "%s%s\n%s"
                      (jtk-project-name)
                      git-info
                      (replace-regexp-in-string project-root ":/" pwd))
            (replace-regexp-in-string home "~" pwd))))
    (format "\n%s\n↳ λ " info)))

(defun jtk-shell-terminal ()
  "Open zsh in a terminal."
  (interactive)
  (eat "/bin/zsh"))

(defun jtk-shell-mistty ()
  "Open zsh in mistty, in the current project's root if there is one."
  (interactive)
  (if (jtk-project-root)
      (mistty-in-project)
    (mistty)))

;; NOTE: A suggestion from the history shows greyed after point while
;; typing, as in fish; TAB takes it.  The history comes first in this
;; buffer's completion, so TAB completes from it too, and from files
;; and commands where the history has nothing; in the form of bbe and
;; clje, Clojure's names come before both.  corfu does not pop up by
;; itself here, so the two do not show at once.
(defun jtk-shell-suggest-from-history ()
  "Turn on suggestions from the history in the current eshell."
  (setq-local completion-at-point-functions
              (append '(jtk-babashka-completion-at-point cape-history)
                      completion-at-point-functions)
              corfu-auto nil)
  (completion-preview-mode 1))

(use-package
  eshell
  :ensure nil
  :commands (eshell)

  :hook
  (eshell-mode . jtk-shell-suggest-from-history)

  :bind
  (:map jtk-run-map
        :prefix-map jtk-shell-map
        :prefix "s"
        :prefix-docstring
        "Shells: eshell, a terminal, zsh in mistty, a single command."
        ("s" . jtk-shell-eshell)
        ("S" . eshell)
        ("t" . jtk-shell-terminal)
        ("z" . jtk-shell-mistty)
        ("c" . shell-command)

   ;; NOTE: <up>, <down>, M-p and M-n go through the history of the
   ;; input typed so far; eshell's history mode binds them.
   :map eshell-mode-map
   ("C-<return>" . end-of-buffer)
   ("M-P" . consult-history)
   ;; NOTE: C-o takes something into a buffer of its own, as it exports
   ;; a list from the minibuffer (init-minibuffer).  eshell's own key
   ;; for inserting a buffer name, C-c M-b, stays.
   ("C-o" . jtk-babashka-edit-form))

  :config
  (require 'em-smart)
  (add-hook 'eshell-named-command-hook #'jtk-babashka-eshell-command)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-hist-ignoredups t
        eshell-history-size 1024
        eshell-rc-script "~/.eshellrc"
        eshell-prompt-function #'jtk-shell-prompt
        eshell-prompt-regexp "^[^λ]*λ\s")

  (defun eshell/repl (&rest profile)
    "Start a Clojure REPL, with the Leiningen PROFILE if given."
    (if (seq-empty-p profile)
        (call-interactively 'jtk-clojure-jack-in-dwim)
      (jtk-clojure-jack-in-with-profile (apply 'concat profile))))

  (defun eshell/open (file)
    "Open FILE with the program the system has for it."
    (browse-url-of-file file)))

;; NOTE: Only the programs in `eshell-visual-commands' and their kin run
;; in eat; the rest of eshell's output stays plain text.
;; `eat-eshell-mode' would put all of it through the terminal, which
;; is slower.
(use-package
  eat
  :commands (eat)
  :hook
  (eshell-load . eat-eshell-visual-command-mode))

;; NOTE: Without its module mistty uses Emacs' own terminal (eterm).
;; `M-x mistty-install' fetches the faster one (alacritty) into the
;; package's build; the module is bound to mistty's version, so after an
;; update mistty falls back to eterm until it is fetched again.
(use-package
  mistty
  :commands (mistty mistty-in-project)
  :custom
  (mistty-shell-command '("/bin/zsh")))

(use-package eshell-up
  :commands
  (eshell-up eshell-up-peek))

(provide 'init-shell)

;;; init-shell.el ends here
