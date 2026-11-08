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
(declare-function cape-history "cape")
(defvar pcomplete-exit-function)
(declare-function pcomplete-default-exit-function "pcomplete")
(declare-function eshell/pwd "em-dirs")
(declare-function magit-anything-modified-p "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function eat "eat")
(declare-function mistty "mistty")
(declare-function mistty-in-project "mistty-project")
(declare-function eshell-insert-buffer-name "esh-arg")

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

;; NOTE: C-o takes something into a buffer of its own, as it exports a
;; list from the minibuffer (init-minibuffer); elsewhere in eshell it
;; keeps inserting a buffer's name, as before.  eshell's own key for
;; that, C-c M-b, works too.
(defun jtk-shell-edit-form-or-insert-buffer-name ()
  "Edit the form of a `bbe' or `clje' command, else insert a buffer name.
The form opens in a Clojure buffer of its own (`jtk-babashka-edit-form');
on any other input this is `eshell-insert-buffer-name'."
  (interactive)
  (call-interactively
   (if (jtk-babashka-eshell-command-line-p)
       #'jtk-babashka-edit-form
     #'eshell-insert-buffer-name)))

;; WATCHOUT: pcomplete adds a space after every completion reported
;; as finished.  TAB on its own never reports a directory so, since one
;; can go on into it; taking the greyed suggestion with TAB, and corfu,
;; always do, and `cd sub' became `cd subdir/ ', with no way on.
(defun jtk-shell-pcomplete-exit (completion status)
  "Finish COMPLETION as pcomplete does, but with no space after a directory.
STATUS is as for `pcomplete-default-exit-function'."
  (unless (string-suffix-p "/" completion)
    (pcomplete-default-exit-function completion status)))

;; NOTE: A suggestion from the history shows greyed after point while
;; typing, as in fish; TAB takes it.  The history comes first in this
;; buffer's completion, so TAB completes from it too, and from files
;; and commands where the history has nothing; in the form of bbe and
;; clje, Clojure's names come before both.  On a path the history keeps
;; out of the way and files complete as usual.  corfu does not pop up
;; by itself here, so the two do not show at once.
(defun jtk-shell--path-at-point-p ()
  "Return non-nil if the word at point looks like a file name.
It does if it holds a slash, starts with ~ or a dot, or begins the name
of a file in a local current directory; a server is not asked, which
would cost a round trip on every key."
  (let ((word (or (thing-at-point 'filename t) "")))
    (and (not (string-empty-p word))
         (or (string-match-p "/" word)
             (string-match-p "\\`[~.]" word)
             (and (not (file-remote-p default-directory))
                  (file-name-all-completions word default-directory))))))

(defun jtk-shell-history-completion-at-point ()
  "Complete from the history, unless the word at point is a path."
  (unless (jtk-shell--path-at-point-p)
    (cape-history)))

(defun jtk-shell-suggest-from-history ()
  "Turn on suggestions from the history in the current eshell."
  (setq-local completion-at-point-functions
              (append '(jtk-babashka-completion-at-point
                        jtk-shell-history-completion-at-point)
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
   ("C-o" . jtk-shell-edit-form-or-insert-buffer-name))

  :config
  (require 'em-smart)
  (require 'pcomplete)
  (setq pcomplete-exit-function #'jtk-shell-pcomplete-exit)
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

;; WATCHOUT: eat runs a program of eshell on a server with its full
;; remote name (/remote:host:/bin/htop), and a script with its remote
;; name as argument; the server finds neither and needs the names as it
;; sees them.  And TRAMP sets TERM=dumb for every program there, over
;; eat's own type, which a server would not know anyway; htop then
;; refuses the terminal.  So on a server the program starts through
;; env with a type servers know.
(defconst jtk-shell-eat-server-term "xterm-256color"
  "Terminal type eat announces to programs on a server.")

(defun jtk-shell--local-name (arg)
  "Return ARG named as on its host, if it is a remote file name."
  (if (and (stringp arg) (file-remote-p arg))
      (file-local-name arg)
    arg))

(defun jtk-shell-eat-server-command (args)
  "Return the ARGS of `eat-exec' fit for a program on a server.
ARGS are BUFFER NAME COMMAND STARTFILE SWITCHES; locally they stay."
  (pcase-let ((`(,buffer ,name ,command ,startfile ,switches) args))
    (if (not (file-remote-p default-directory))
        args
      (list buffer name "env" startfile
            (append (list "-u" "TERMINFO"
                          (concat "TERM=" jtk-shell-eat-server-term)
                          (jtk-shell--local-name command))
                    (mapcar #'jtk-shell--local-name switches))))))

;; NOTE: Only the programs in `eshell-visual-commands' and their kin run
;; in eat; the rest of eshell's output stays plain text.
;; `eat-eshell-mode' would put all of it through the terminal, which
;; is slower.
(use-package
  eat
  :commands (eat)

  :hook
  (eshell-load . eat-eshell-visual-command-mode)

  :config
  (advice-add 'eat-exec :filter-args #'jtk-shell-eat-server-command))

;; NOTE: Without its module mistty uses Emacs' own terminal (eterm).
;; `M-x mistty-install' fetches the faster one (alacritty) into the
;; package's build; the module is bound to mistty's version, so after an
;; update mistty falls back to eterm until it is fetched again.
(use-package
  mistty
  :commands (mistty mistty-in-project)
  :custom
  (mistty-shell-command '("/bin/zsh"))

  :config
  ;; NOTE: On a server mistty starts the server's shell, which need not
  ;; be zsh: there the command is unset, and mistty takes the shell
  ;; TRAMP knows for the connection.
  (connection-local-set-profile-variables
   'jtk-shell-mistty-on-server
   '((mistty-shell-command . nil)))
  (connection-local-set-profiles
   '(:application tramp) 'jtk-shell-mistty-on-server))

(use-package eshell-up
  :commands
  (eshell-up eshell-up-peek))

(provide 'init-shell)

;;; init-shell.el ends here
