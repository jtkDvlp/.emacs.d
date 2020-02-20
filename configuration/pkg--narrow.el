(defun narrow/narrow-dwim (beginning end)
  (interactive "r")
  (if (use-region-p)
      (narrow-to-region)
    (narrow-to-defun t)))

(defalias 'narrow/widen 'widen)

(provide 'pkg--narrow)
