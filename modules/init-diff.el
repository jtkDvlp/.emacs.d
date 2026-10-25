(use-package
  ediff
  :ensure nil

  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)
  (with-elscreen #'ediff-buffers #'ediff-quit)
  (with-elscreen #'ediff-files #'ediff-quit)
  (with-elscreen #'ediff-directories #'ediff-quit)

  :commands
  (ediff-buffers
   ediff-files
   ediff))

(use-package
  ztree
  :demand t

  :commands
  (ztree-diff)

  :init
  (defalias 'ediff-trees 'ztree-diff)

  (setq ztree-diff-filter-list '("^\\.$" "^\\.\\.$" "^\\.git$" "^\\.DS_Store$" "^node_modules$")
        ztree-draw-unicode-lines t))

(provide 'init-diff)
