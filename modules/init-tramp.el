;;; init-tramp.el --- Files and programs on servers -*- lexical-binding: t; -*-

;;; Commentary:

;; TRAMP for servers reached by ssh, in the LAN and outside: fewer round
;; trips to the server for opening and saving files, listing directories
;; and starting programs.  Each command TRAMP sends costs one round trip,
;; which on a distant server is what makes TRAMP slow.  Emacs' defaults
;; already copy large files with scp (the default method where scp is
;; installed) and keep auto-save files local.

;;; Code:

(defvar vc-ignore-dir-regexp)
(defvar tramp-file-name-regexp)
(declare-function tramp-ssh-or-plink-options "tramp-sh")

;; NOTE: With a local and a remote directory side by side in dired,
;; copying (C) and moving (R) offer the other window's directory, which
;; makes copying to or from a server one key, as scp does.
(setopt dired-dwim-target t)

(use-package
  tramp
  :ensure nil

  :custom
  ;; NOTE: No lock files on the server; no second Emacs edits the same
  ;; file there.
  (remote-file-name-inhibit-locks t)
  ;; NOTE: A copy between two servers goes directly, not through here.
  (tramp-use-scp-direct-remote-copying t)

  :config
  ;; NOTE: /remote:host:/path is the preferred method under a name of its
  ;; own, today scp: ssh to log in, scp for files above
  ;; `tramp-copy-size-limit'.  A later change of method is one line here.
  (add-to-list 'tramp-methods
               (cons "remote" (cdr (assoc "scp" tramp-methods))))

  ;; NOTE: Programs (eshell, compile, magit, eglot) start through ssh
  ;; directly instead of through TRAMP's shell on the server.
  ;; WATCHOUT: That needs a login without asking: an ssh key, ssh-agent,
  ;; or TRAMP's open connection, which it shares (ControlMaster).  Over
  ;; several hops TRAMP falls back to its shell by itself.
  (connection-local-set-profile-variables
   'jtk-tramp-direct-async
   '((tramp-direct-async-process . t)))
  (dolist (protocol '("ssh" "scp" "remote"))
    (connection-local-set-profiles
     `(:application tramp :protocol ,protocol)
     'jtk-tramp-direct-async))

  ;; WATCHOUT: Emacs 31.1's TRAMP asks a function
  ;; `tramp-ssh-controlmaster-options' for the ssh options of a program
  ;; started directly, but tramp-sh now calls it
  ;; `tramp-ssh-or-plink-options'; the call finds nothing and stays
  ;; silent.  Without the alias the program opens its own ssh connection
  ;; instead of sharing TRAMP's, which cannot ask for a password and
  ;; fails wherever the login needs one.
  (with-eval-after-load 'tramp-sh
    (unless (fboundp 'tramp-ssh-controlmaster-options)
      (defalias 'tramp-ssh-controlmaster-options
        #'tramp-ssh-or-plink-options)))

  ;; NOTE: dired checks no symbolic link on a server, one round trip
  ;; each; broken links are no longer marked there.
  (connection-local-set-profile-variables
   'jtk-tramp-dired
   '((dired-check-symlinks . nil)))
  (connection-local-set-profiles '(:application tramp) 'jtk-tramp-dired)

  ;; NOTE: No version control for files on a server: it asked git on
  ;; every file opened there.  magit works there all the same.
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp tramp-file-name-regexp)))

(provide 'init-tramp)

;;; init-tramp.el ends here
