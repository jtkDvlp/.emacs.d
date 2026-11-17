;;; init-shell.el --- Shells -*- lexical-binding: t; -*-

;;; Commentary:

;; eshell with a project-aware prompt, history and commands, a
;; suggestion from the history while typing, highlighting of the input
;; and colors from programs; aliases are in etc/eshell/aliases.  ghostel
;; is the terminal: zsh on its own, programs in eshell that need a
;; terminal (top, less, ...), and any command with v in front.
;; mistty runs zsh too, but edits its command line with Emacs' keys.
;; In eshell, bbe and clje evaluate Clojure with a local Babashka, also
;; on data from a remote host (jtk-babashka).

;;; Code:

(require 'jtk-project)
(require 'jtk-babashka)

(defvar corfu-auto)
(declare-function cape-history "cape")
(defvar pcomplete-exit-function)
(defvar eshell-variable-aliases-list)
(declare-function pcomplete-default-exit-function "pcomplete")
(declare-function eshell/pwd "em-dirs")
(declare-function magit-anything-modified-p "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function ghostel "ghostel")
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
  (ghostel))

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
  "Edit the form of the `bbe' or `clje' at point, else insert a buffer name.
The form opens in a Clojure buffer of its own (`jtk-babashka-edit-form');
on any other input this is `eshell-insert-buffer-name'."
  (interactive)
  (call-interactively
   (if (jtk-babashka-eshell-command-at-point-p)
       #'jtk-babashka-edit-form
     #'eshell-insert-buffer-name)))

;; NOTE: Programs see a terminal that shows colors but cannot move the
;; cursor, so git, grep and others color their output again, which
;; eshell turns into faces.  Under TERM=dumb, or none at all, as from a
;; GUI Emacs, they leave it plain.  Full-screen programs run in ghostel,
;; which sets its own TERM.
;; WATCHOUT: Through `eshell-variable-aliases-list', which eshell reads
;; at every program start, not through the buffer's
;; `process-environment': envrc resets that in every eshell, with or
;; without an .envrc, and TERM was dumb again.
(defconst jtk-shell-term "dumb-emacs-ansi"
  "Terminal type programs in eshell see.")

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
  (add-to-list 'eshell-variable-aliases-list '("TERM" jtk-shell-term t))
  (advice-add 'eshell/cd :filter-args #'jtk-shell-cd-on-server)
  (add-hook 'eshell-named-command-hook #'jtk-babashka-eshell-command)
  (dolist (name jtk-babashka-eshell-commands)
    (defalias (intern (concat "eshell/" name))
      #'jtk-babashka-eshell-placeholder))

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-hist-ignoredups t
        eshell-history-size 1024
        eshell-rc-script "~/.eshellrc"
        eshell-prompt-function #'jtk-shell-prompt)

  (defun eshell/repl (&rest profile)
    "Start a Clojure REPL, with the Leiningen PROFILE if given."
    (if (seq-empty-p profile)
        (call-interactively 'jtk-clojure-jack-in-dwim)
      (jtk-clojure-jack-in-with-profile (apply 'concat profile))))

  (defun eshell/open (file)
    "Open FILE with the program the system has for it."
    (browse-url-of-file file)))

;; WATCHOUT: eshell replaces ~ by this machine's home before cd sees
;; it, so on a server ~ has to be told by that name.
(defun jtk-shell--home-as-tilde (dir)
  "Return DIR with this machine's home directory written as ~."
  (let ((home (directory-file-name (expand-file-name "~"))))
    (if (or (string= dir home) (string-prefix-p (concat home "/") dir))
        (concat "~" (substring dir (length home)))
      dir)))

;; NOTE: On a server, cd stays there as in a shell on the server: an
;; absolute directory, ~ and cd alone are the server's, where eshell
;; would take them as this machine's.  /local: in front goes to this
;; machine, as it runs a command here.
(defun jtk-shell-cd-on-server (args)
  "Return the ARGS of `eshell/cd' with directories on the current host.
/local:DIR is DIR on this machine, /local: alone its home directory."
  (let ((dir (car args))
        (server (file-remote-p default-directory)))
    (cond
     ((and (stringp dir) (string-prefix-p "/local:" dir))
      (cons (expand-file-name (let ((local (substring dir 7)))
                                (if (string-empty-p local) "~" local)))
            (cdr args)))
     ((not server) args)
     ((null args) (list (concat server "~")))
     ((and (stringp dir)
           (not (file-remote-p dir))
           (string-match-p "\\`[/~]" dir))
      (cons (concat server (jtk-shell--home-as-tilde dir)) (cdr args)))
     (t args))))

;; NOTE: Programs that need a terminal run in ghostel: those of
;; `eshell-visual-commands' and their kin by themselves, any other with
;; v (for visual) in front.  The rest of eshell's output stays plain
;; text; through a terminal it took 45 times as long (100,000 lines:
;; 1.9 s with eat instead of 0.04 s) and kept only the last 131 KB.  As
;; a terminal ghostel is 15 times as fast as eat (0.1 s against 1.6 s;
;; Emacs 31.1, cloud, batch).
;; WATCHOUT: The native module is built from source on this machine
;; (Zig 0.17, `M-x ghostel-module-compile'), never downloaded: ghostel
;; loads a downloaded one without checking a hash or signature.  It
;; lives under var/, where elpaca's rebuilds leave it, and has to be
;; built again when an update of ghostel asks for a newer one.
(use-package
  ghostel
  ;; NOTE: ghostel-module-compile has no autoload of its own.
  :commands (ghostel ghostel-exec ghostel-module-compile)

  :hook
  (eshell-load . ghostel-eshell-visual-command-mode)

  :init
  (setq ghostel-module-directory
        (no-littering-expand-var-file-name "ghostel/"))

  ;; WATCHOUT: Not ghostel's own terminal type, xterm-ghostty: its
  ;; entry is found only through TERMINFO, which ncurses ignores in
  ;; privileged programs.  macOS' top failed with "Error opening
  ;; terminal: xterm-ghostty", while infocmp read the entry there.
  ;; Every system and server knows xterm-256color; given up are extras
  ;; such as synchronized output.
  :custom
  (ghostel-module-auto-install 'compile)
  (ghostel-shell "/bin/zsh")
  (ghostel-term "xterm-256color")

  :config
  ;; NOTE: /remote: (init-tramp) starts the server's login shell, as
  ;; ssh and scp do.
  (add-to-list 'ghostel-tramp-shells '("remote" login-shell)))

(defalias 'eshell/v #'eshell/ghostel)

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

;; NOTE: Highlights the input while typing: commands, Lisp functions,
;; aliases, options, strings, variables, files; unknown commands in
;; their own face.  Not in a directory on a server, where each check
;; would cost a round trip (the package's default).
(use-package
  eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode 1))

(use-package eshell-up
  :commands
  (eshell-up eshell-up-peek))

(provide 'init-shell)

;;; init-shell.el ends here
