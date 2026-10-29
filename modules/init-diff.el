;;; init-diff.el --- Comparing files -*- lexical-binding: t; -*-

;;; Commentary:

;; ediff, each session in a tab of its own, and ztree for
;; directories.

;;; Code:

(use-package
  ediff
  :ensure nil

  :init
  (defun jtk-diff-close-tab ()
    "Close the tab of the ediff session that just ended."
    (tab-bar-close-tab))

  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)

  ;; NOTE: Each session gets a tab of its own, closed when the session
  ;; ends, after ediff's own cleanup.  The quit hook does not run when
  ;; the question whether to quit is answered with n.
  (add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
  (add-hook 'ediff-quit-hook #'jtk-diff-close-tab 90)

  :commands
  (ediff-buffers
   ediff-files
   ediff))

(use-package
  ztree
  :commands
  (ztree-diff)

  :init
  (defalias 'ediff-trees 'ztree-diff)

  ;; NOTE: Both are plain variables, not options, so `:custom' would not
  ;; reach them; set before loading, they become the defaults.
  (setq ztree-diff-filter-list
        '("^\\.$" "^\\.\\.$" "^\\.git$" "^\\.DS_Store$" "^node_modules$")
        ztree-draw-unicode-lines t))

(provide 'init-diff)

;;; init-diff.el ends here
