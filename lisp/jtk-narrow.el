;;; jtk-narrow.el --- Narrowing -*- lexical-binding: t; -*-

;;; Commentary:

;; Narrow to the region if active, else to the defun.

;;; Code:

(defun jtk-narrow-dwim (beginning end)
  (interactive "r")
  (if (use-region-p)
      (narrow-to-region)
    (narrow-to-defun t)))

(defalias 'jtk-narrow-widen 'widen)

(provide 'jtk-narrow)

;;; jtk-narrow.el ends here
