;;; jtk-zoom.el --- Font size steps -*- lexical-binding: t; -*-

;;; Commentary:

;; Set the font height of the frame to fixed sizes, or step it up and
;; down.

;;; Code:

(defun jtk-zoom--apply (n)
  (set-face-attribute 'default (selected-frame) :height n))

(defun jtk-zoom-default ()
  (interactive)
  (jtk-zoom--apply 100))

(defun jtk-zoom-custom-1 ()
  (interactive)
  (jtk-zoom--apply 150))

(defun jtk-zoom-custom-2 ()
  (interactive)
  (jtk-zoom--apply 200))

(defun jtk-zoom-custom-3 ()
  (interactive)
  (jtk-zoom--apply 300))

(defun jtk-zoom--modify (n)
  (let ((factor 10))
    (jtk-zoom--apply
     (+ (face-attribute 'default :height)
        (* n factor)))))

(defun jtk-zoom-inc ()
  (interactive)
  (jtk-zoom--modify 1))

(defun jtk-zoom-dec ()
  (interactive)
  (jtk-zoom--modify -1))

(provide 'jtk-zoom)

;;; jtk-zoom.el ends here
