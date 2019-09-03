(defun zoom/apply (n)
  (set-face-attribute 'default (selected-frame) :height n))

(defun zoom/default ()
  (interactive)
  (zoom/apply 100))

(defun zoom/custom-1 ()
  (interactive)
  (zoom/apply 150))

(defun zoom/custom-2 ()
  (interactive)
  (zoom/apply 200))

(defun zoom/custom-3 ()
  (interactive)
  (zoom/apply 300))

(defun zoom/modify (n)
  (let ((factor 10))
    (zoom/apply
     (+ (face-attribute 'default :height)
        (* n factor)))))

(defun zoom/inc ()
  (interactive)
  (zoom/modify 1))

(defun zoom/dec ()
  (interactive)
  (zoom/modify -1))

(provide 'pkg--zoom)
