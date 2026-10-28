;;; jtk-project.el --- The current project -*- lexical-binding: t; -*-

;;; Commentary:

;; Root and name of the project the current buffer belongs to, as
;; project.el sees it, or nil outside a project; and a choice of what to
;; do in a project, which modules add to by the kind of project.

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

(defvar jtk-project-actions nil
  "What `jtk-project-act' offers, as a list of (LABEL COMMAND MARKERS).
LABEL is shown in the list and COMMAND run in the project's root.
MARKERS, a list of file names, offers the action only in projects whose
root holds one of them; without it the action is offered everywhere.")

(defun jtk-project--action-p (root action)
  "Return non-nil if ACTION is offered in the project at ROOT."
  (let ((markers (nth 2 action)))
    (or (null markers)
        (seq-some (lambda (marker)
                    (file-exists-p (expand-file-name marker root)))
                  markers))))

(defun jtk-project-act ()
  "Choose one of `jtk-project-actions' and run it in the current project."
  (interactive)
  (let* ((root
          (jtk-project-root))

         (actions
          (seq-filter (apply-partially #'jtk-project--action-p root)
                      jtk-project-actions))

         (label
          (completing-read (format "In %s: " (jtk-project-name))
                           (mapcar #'car actions) nil t))

         ;; NOTE: Some commands, CIDER's among them, take the project
         ;; from the current directory, not from project.el.
         (default-directory
          root))

    (call-interactively (nth 1 (assoc label actions)))))

(provide 'jtk-project)

;;; jtk-project.el ends here
