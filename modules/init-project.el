;;; init-project.el --- Projects -*- lexical-binding: t; -*-

;;; Commentary:

;; project.el: switching projects and finding files, directories and
;; buffers within one.  Emacs' own project keys are under C-x p; magit,
;; consult, xref and eglot ask project.el what the project is.  What to
;; do after switching is `jtk-project-actions'; languages add theirs.

;;; Code:

(require 'jtk-project)

(use-package
  project
  :ensure nil

  :custom
  ;; NOTE: As with projectile, a .projectile file marks a project root,
  ;; also within a git repository.
  (project-vc-extra-root-markers '(".projectile"))
  ;; NOTE: Files git ignores are left out anyway; these directories stay
  ;; out where git does not ignore them.
  (project-vc-ignores '("node_modules/" "dist/" "elpa/" "cache/" ".cache/"))
  ;; NOTE: A list to choose from, as everywhere, instead of project.el's
  ;; one-key menu in the echo area.
  (project-switch-commands #'jtk-project-act)

  :config
  (defun save-all-buffers ()
    (interactive)
    (save-some-buffers t))

  :bind
  (("C-x b" . consult-buffer)
   ("C-x B" . consult-project-buffer)

   ("C-x f" . find-file)
   ("C-x F" . project-find-file)

   ("C-x C-f" . consult-recent-file)
   ;; NOTE: The project's buffers list its recent files as well.
   ("C-x C-S-F" . consult-project-buffer)

   ("C-x d" . project-dired)
   ("C-x D" . project-find-dir)

   ("C-x C-d" . nil)

   ;; NOTE: Asks for the project, then what to do there.
   ("C-x x f" . project-switch-project)
   ("C-x x F" . consult-project-buffer)

   ("C-q" . switch-to-prev-buffer)

   ("C-x C-s" . save-buffer)
   ("C-x C-S-S" . save-all-buffers)

   ("C-x k" . kill-buffer)
   ("C-x K" . project-kill-buffers)

   :map project-prefix-map
   ("m" . magit-project-status)))

(dolist (action
         '(("Find file" project-find-file)
           ("Find directory" project-find-dir)
           ("Buffers and recent files" consult-project-buffer)
           ("Search with git grep" consult-git-grep)
           ("Magit" magit-project-status)
           ("Eshell" project-eshell)
           ("Dired" project-dired)
           ("Any other command, by its key" project-any-command)))
  (add-to-list 'jtk-project-actions action t))

(provide 'init-project)

;;; init-project.el ends here
