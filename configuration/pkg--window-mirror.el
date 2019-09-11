(defvar window-mirror--mirrors '())
(defvar window-mirror--current-pairing-master nil)

(defun window-mirror/start-mirroring ()
  "Enables window-mirror-mode and starts pairing."
  (interactive)
  (unless (bound-and-true-p window-mirror)
    (window-mirror)
    (window-mirror/start-pairing)))

(defun window-mirror/start-pairing ()
  "Start pairing (current window becomes master)."
  (interactive)
  (let ((window (selected-window)))
    (if (assoc window window-mirror--mirrors)
	(message "window-mirror: window already mirrors")
      (if window-mirror--current-pairing-master
	  (message "window-mirror: other pairing already started")
	(setq window-mirror--current-pairing-master window)
	(message "window-mirror: pairing started")))))

(defun window-mirror/cancel-pairing ()
  "Cancel pairing."
  (interactive)
  (if window-mirror--current-pairing-master
      (progn
	(setq window-mirror--current-pairing-master nil)
	(message "window-mirror: pairing canceled"))
    (message "window-mirror: no pairing started")))

(defun window-mirror/pair ()
  "Complete pairing (current window becomes slave)."
  (interactive)
  (if window-mirror--current-pairing-master
      (progn
	(add-to-list
	 'window-mirror--mirrors
	 (cons window-mirror--current-pairing-master (selected-window)))
	(setq window-mirror--current-pairing-master nil)
	(message "window-mirror: pairing completed"))
    (message "window-mirror: first start pairing")))

(defun window-mirror/unpair ()
  "Terminate pairing."
  (interactive)
  (let* ((window (selected-window))
	 (mirror (assoc window window-mirror--mirrors))
	 (mirror (if mirror mirror (rassoc window window-mirror--mirrors))))
    (if mirror
	(progn
	  (setq window-mirror--mirrors
		(assq-delete-all (car mirror) window-mirror--mirrors))
	  (message "window-mirror: pairing terminated"))
      (message "window-mirror: no pairing found"))))

(defun window-mirror/mirror ()
  "Mirror current window to slave if paired."
  (let ((mirror (assoc (selected-window) window-mirror--mirrors)))
    (when mirror
      (let ((master-buffer (current-buffer))
	    (master-point (point))
	    
	    (slave (cdr mirror)))
	
	(with-selected-window slave
	  (unless (equal master-buffer (current-buffer))
	    (switch-to-buffer master-buffer t t))
	  (unless (equal master-point (point))
	    (goto-char master-point)))))))

(define-minor-mode window-mirror
  "Global minor mode for mirroring windows."
  nil
  (:eval
   (if (and (equal '() window-mirror--mirrors)
	    (equal nil window-mirror--current-pairing-master))
       "mirror(!)"
     (let ((window (selected-window)))
       (if (or (equal window window-mirror--current-pairing-master)
	       (assoc window window-mirror--mirrors))
	   "mirror(m)"
	 (if (rassoc window window-mirror--mirrors)
	     "mirror(s)"
	   "")))))
  :global t
  (if (bound-and-true-p window-mirror) 
      (add-hook 'post-command-hook 'window-mirror/mirror t)
    (remove-hook 'post-command-hook 'window-mirror/mirror)))

(provide 'pkg--window-mirror)
