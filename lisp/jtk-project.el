;;; jtk-project.el --- The current project -*- lexical-binding: t; -*-

;;; Commentary:

;; Root and name of the project the current buffer belongs to, as
;; project.el sees it, or nil outside a project; the kinds of a project
;; (Clojure, npm, ...), told by marker files, with the commands each
;; kind brings; and what embark needs to act on a project from a list.

;;; Code:

(require 'project)

(defun jtk-project-root ()
  "Return the root directory of the current project, or nil.
The name is absolute and ends in a slash, like `buffer-file-name' and
unlike `project-root', which abbreviates the home directory to ~."
  (when-let* ((project (project-current)))
    (expand-file-name (project-root project))))

(defun jtk-project-name ()
  "Return the name of the current project, or nil."
  (when-let* ((project (project-current)))
    (project-name project)))

(defvar jtk-project-kinds nil
  "Kinds of projects, as a list of (NAME MARKERS KEYMAP FEATURE).
A project is of kind NAME, a string, if its root holds one of the files
MARKERS.  KEYMAP is a symbol whose value holds the commands for that
kind.  FEATURE, if non-nil, is required before KEYMAP is used, for the
keymap of a package that may not be loaded yet.")

(defun jtk-project-kinds-of (root)
  "Return the entries of `jtk-project-kinds' for the project at ROOT."
  (seq-filter (lambda (kind)
                (seq-some (lambda (marker)
                            (file-exists-p (expand-file-name marker root)))
                          (nth 1 kind)))
              jtk-project-kinds))

(defun jtk-project--kinds-keymap (kinds)
  "Return one keymap with the commands of all KINDS."
  (make-composed-keymap
   (mapcar (lambda (kind)
             (when-let* ((feature (nth 3 kind)))
               (require feature))
             (symbol-value (nth 2 kind)))
           kinds)))

(defun jtk-project-run-kind-command ()
  "Run a command for the kinds of the current project, chosen by its key.
C-h instead lists the commands with their keys to choose from."
  (interactive)
  (let* ((kinds
          (or (when-let* ((root (jtk-project-root)))
                (jtk-project-kinds-of root))
              (user-error "No commands for this kind of project")))

         (keymap
          (jtk-project--kinds-keymap kinds))

         ;; WATCHOUT: C-h is a prefix in the global map, so reading a key
         ;; sequence would wait for the key after it; a binding of its
         ;; own here ends the sequence at C-h.
         (keys
          (let ((overriding-terminal-local-map
                 (make-composed-keymap (define-keymap "C-h" #'ignore)
                                       keymap)))
            (read-key-sequence-vector
             (format "%s (C-h: list): " (mapconcat #'car kinds ", "))))))

    (if (equal (key-description keys) "C-h")
        (embark-bindings-in-keymap keymap)
      (let ((command (lookup-key keymap keys)))
        (unless (commandp command)
          (user-error "%s runs nothing here" (key-description keys)))
        (call-interactively command)))))

(defun jtk-project-annotate (candidate)
  "Return the kinds of the project directory CANDIDATE, for marginalia."
  (when (file-directory-p candidate)
    (when-let* ((kinds (jtk-project-kinds-of candidate)))
      (concat " " (propertize (mapconcat #'car kinds ", ")
                              'face 'completions-annotations)))))

(defun jtk-project-act-in-target (&rest arguments)
  "Run an embark action on a project with that project as the current one.
Meant for `embark-around-action-hooks'; ARGUMENTS are the hook's, with
:run, :type and :target."
  (if (eq (plist-get arguments :type) 'jtk-project)
      (let* ((directory (expand-file-name (plist-get arguments :target)))
             (project-current-directory-override directory)
             (default-directory directory))
        (apply (plist-get arguments :run) arguments))
    (apply (plist-get arguments :run) arguments)))

(provide 'jtk-project)

;;; jtk-project.el ends here
