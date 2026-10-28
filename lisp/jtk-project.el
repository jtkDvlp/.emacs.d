;;; jtk-project.el --- The current project -*- lexical-binding: t; -*-

;;; Commentary:

;; Root and name of the project the current buffer belongs to, as
;; project.el sees it, or nil outside a project.

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

(provide 'jtk-project)

;;; jtk-project.el ends here
