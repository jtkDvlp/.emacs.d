;;; jtk-project.el --- The current project -*- lexical-binding: t; -*-

;;; Commentary:

;; Root and name of the project the current buffer belongs to, as
;; project.el sees it, or nil outside a project; and what embark needs
;; to act on a project from a list.

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
