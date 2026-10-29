;;; init-minibuffer.el --- Minibuffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; How choices are made in the minibuffer, and the commands built on
;; that: vertico shows the candidates, orderless matches them,
;; marginalia annotates them, consult adds commands with preview
;; (lines, buffers, grep, kill ring, history), and embark exports a
;; candidate list into a buffer (C-o) or acts on the marked candidate
;; (M-x).  All of them work on `completing-read',
;; so every command that asks for a choice uses them without setup of
;; its own.  Which keys open files and buffers is in init-project.

;;; Code:

(setopt enable-recursive-minibuffers t)

;; NOTE: Minibuffer history survives restarts; vertico sorts by it.
(savehist-mode)

(use-package
  vertico
  :demand t
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  :config
  (vertico-mode))

(use-package
  orderless
  :demand t
  :custom
  ;; NOTE: Space separates terms that match in any order, as with ivy.
  ;; File names keep `partial-completion', so that /u/s/l still expands
  ;; to /usr/share/local.
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package
  marginalia
  :demand t
  :config
  (marginalia-mode))

(declare-function consult-git-grep "consult")

(defun jtk-git-grep-dwim ()
  "Search the git repository, starting with the active region if any."
  (interactive)
  (let ((text (when mark-active
                (buffer-substring-no-properties (region-beginning)
                                                (region-end)))))
    (deactivate-mark)
    (consult-git-grep nil text)))

(use-package
  consult
  :bind
  (("C-s" . consult-line)
   ("C-r" . consult-line)

   ;; NOTE: Both keys search the same way. Replacing goes through the
   ;; export: C-o, then `e' in the grep buffer, C-c C-c to apply.
   ("C-S-s" . jtk-git-grep-dwim)
   ("C-S-r" . jtk-git-grep-dwim)

   ("C-S-Y" . consult-yank-pop)

   ;; NOTE: Searching the history of the prompt is M-P everywhere: here,
   ;; in eshell and in the CIDER REPL.
   :map minibuffer-local-map
   ("M-P" . consult-history)

   ;; NOTE: Does nothing here; the global C-r would search the lines of
   ;; the prompt.
   ("C-r" . ignore)))

(use-package
  embark
  :custom
  ;; NOTE: C-h after a prefix lists its keys to choose from, with their
  ;; descriptions; it stands in for which-key.
  (prefix-help-command #'embark-prefix-help-command)
  ;; NOTE: The actions show only on C-h after M-x, no list pops up by
  ;; itself.  In that list, as in every C-h list, @ and a key run that
  ;; key's command directly.
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))

  :init
  ;; NOTE: In a list exported with C-o that has no buffer of its own
  ;; kind (dired, occur, grep), the action keys work directly on the
  ;; entry under the cursor, as after M-x in the minibuffer: e on a
  ;; project opens eshell there.  C-h on an entry lists them, M-a turns
  ;; it off.
  ;; WATCHOUT: embark runs the mode hook before it inserts the entries,
  ;; and the minor mode only binds the keys on entries that exist; hence
  ;; the timer, which runs once the command has filled the buffer.
  (defun jtk-embark-collect-act-directly ()
    (let ((buffer (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (embark-collect-direct-action-minor-mode 1)))))))

  :hook
  (embark-collect-mode . jtk-embark-collect-act-directly)

  :bind
  (:map minibuffer-local-map
        ("C-o" . embark-export)
        ;; NOTE: x for execute: other things to do with the marked entry
        ;; than RET does; for a project, the project commands
        ;; (init-project).  Within a prompt M-x no longer starts another
        ;; M-x; on a project, M-x x runs any command in it.
        ("M-x" . embark-act)

        ;; NOTE: All keys of the buffer as a list to search, instead of
        ;; the help buffer; with C-u also the global ones.
        :map help-map
        ("b" . embark-bindings)))

;; NOTE: Teaches embark to export consult's lists: lines to an occur
;; buffer, grep results to a grep buffer.  Both are editable (`e').
(use-package
  embark-consult
  :after (embark consult))

;; NOTE: In exported lines and grep results the file follows point, also
;; while editing them: moving to a line shows it in its file.  C-c C-f
;; toggles.  occur-edit-mode resets local modes, hence its own hook.
(dolist (hook '(occur-mode-hook occur-edit-mode-hook grep-mode-hook))
  (add-hook hook #'next-error-follow-minor-mode))

(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
