;;; init-project.el --- Projects -*- lexical-binding: t; -*-

;;; Commentary:

;; project.el: switching projects and finding files, directories and
;; buffers within one; the project commands on C-x x p, and on a project
;; in the project list with M-o (embark).  magit, consult, xref and eglot
;; ask project.el what the project is.

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
  ;; NOTE: RET in the project list finds a file; anything else is M-o
  ;; on the project (see below).
  (project-switch-commands #'project-find-file)

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

   ("C-x x f" . project-switch-project)
   ("C-x x F" . consult-project-buffer)

   ("C-q" . switch-to-prev-buffer)

   ("C-x C-s" . save-buffer)
   ("C-x C-S-S" . save-all-buffers)

   ("C-x k" . kill-buffer)
   ("C-x K" . project-kill-buffers)))

;; NOTE: Emacs' project commands (C-x p), with consult and Magit where
;; they have something better.  `:prefix' cannot be combined with
;; `:parent', hence `fset'.
(defvar-keymap jtk-project-map
  :doc "Commands for the current project."
  :parent project-prefix-map
  "b" #'consult-project-buffer
  "g" #'consult-git-grep
  "m" #'magit-project-status
  "v" #'magit-project-status)
(fset 'jtk-project-map jtk-project-map)
(keymap-set ctl-x-x-map "p" 'jtk-project-map)

(defvar marginalia-command-categories)
(defvar embark-keymap-alist)
(defvar embark-around-action-hooks)

;; NOTE: The candidates of the project list are directories of category
;; `project-file', which embark treats as files.  Their own category
;; gives them `jtk-project-map' as actions, run in the marked project.
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-command-categories
               '(project-switch-project . jtk-project)))

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(jtk-project jtk-project-map))
  (add-to-list 'embark-around-action-hooks
               '(:always jtk-project-act-in-target)))

(provide 'init-project)

;;; init-project.el ends here
