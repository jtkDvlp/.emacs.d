;;; init-minibuffer.el --- Minibuffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; How choices are made in the minibuffer, and the commands built on
;; that: vertico shows the candidates, orderless matches them,
;; marginalia annotates them, consult adds commands with preview
;; (lines, buffers, grep, kill ring, history) and embark exports a
;; candidate list into a buffer.  All of them work on `completing-read',
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
(declare-function projectile-project-root "projectile")

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
  :custom
  ;; NOTE: projectile decides what a project is, for the project
  ;; buffers of `consult-buffer' and `consult-project-buffer' too.
  (consult-project-function (lambda (_) (projectile-project-root)))

  :bind
  (("C-s" . consult-line)
   ("C-r" . consult-line)

   ;; NOTE: Both keys search the same way. Replacing goes through the
   ;; export: C-o, then `e' in the grep buffer, C-c C-c to apply.
   ("C-S-s" . jtk-git-grep-dwim)
   ("C-S-r" . jtk-git-grep-dwim))

  :bind*
  (("C-S-Y" . consult-yank-pop)

   ;; NOTE: As counsel had it: search the history of the current prompt.
   :map minibuffer-local-map
   ("C-r" . consult-history)))

(use-package
  embark
  :bind
  (:map minibuffer-local-map
        ("C-o" . embark-export)))

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
