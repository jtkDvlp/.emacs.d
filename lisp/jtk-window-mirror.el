;;; jtk-window-mirror.el --- Mirror a window -*- lexical-binding: t; -*-

;;; Commentary:

;; A global minor mode that keeps a second window on the buffer and
;; point of a first one, e.g. for pairing on two screens.

;;; Code:

(defvar jtk-window-mirror--mirrors '())
(defvar jtk-window-mirror--current-pairing-master nil)

(defun jtk-window-mirror-start-mirroring ()
  "Enable `jtk-window-mirror-mode' and start pairing."
  (interactive)
  (unless (bound-and-true-p jtk-window-mirror-mode)
    (jtk-window-mirror-mode)
    (jtk-window-mirror-start-pairing)))

(defun jtk-window-mirror-start-pairing ()
  "Start pairing (current window becomes master)."
  (interactive)
  (let ((window (selected-window)))
    (if (assoc window jtk-window-mirror--mirrors)
        (message "window-mirror: window already mirrors")
      (if jtk-window-mirror--current-pairing-master
          (message "window-mirror: other pairing already started")
        (setq jtk-window-mirror--current-pairing-master window)
        (message "window-mirror: pairing started")))))

(defun jtk-window-mirror-cancel-pairing ()
  "Cancel pairing."
  (interactive)
  (if jtk-window-mirror--current-pairing-master
      (progn
        (setq jtk-window-mirror--current-pairing-master nil)
        (message "window-mirror: pairing canceled"))
    (message "window-mirror: no pairing started")))

(defun jtk-window-mirror-pair ()
  "Complete pairing (current window becomes slave)."
  (interactive)
  (if jtk-window-mirror--current-pairing-master
      (progn
        (add-to-list
         'jtk-window-mirror--mirrors
         (cons jtk-window-mirror--current-pairing-master (selected-window)))
        (setq jtk-window-mirror--current-pairing-master nil)
        (message "window-mirror: pairing completed"))
    (message "window-mirror: first start pairing")))

(defun jtk-window-mirror-unpair ()
  "Terminate pairing."
  (interactive)
  (let* ((window (selected-window))
         (mirror (assoc window jtk-window-mirror--mirrors))
         (mirror (if mirror mirror (rassoc window jtk-window-mirror--mirrors))))
    (if mirror
        (progn
          (setq jtk-window-mirror--mirrors
                (assq-delete-all (car mirror) jtk-window-mirror--mirrors))
          (message "window-mirror: pairing terminated"))
      (message "window-mirror: no pairing found"))))

(defun jtk-window-mirror--mirror ()
  "Mirror current window to slave if paired."
  (let ((mirror (assoc (selected-window) jtk-window-mirror--mirrors)))
    (when mirror
      (let ((master-buffer (current-buffer))
            (master-point (point))
            
            (slave (cdr mirror)))
        
        (with-selected-window slave
          (unless (equal master-buffer (current-buffer))
            (switch-to-buffer master-buffer t t))
          (unless (equal master-point (point))
            (goto-char master-point)))))))

(define-minor-mode jtk-window-mirror-mode
  "Global minor mode for mirroring windows."
  nil
  (:eval
   (if (and (equal '() jtk-window-mirror--mirrors)
            (equal nil jtk-window-mirror--current-pairing-master))
       "mirror(!)"
     (let ((window (selected-window)))
       (if (or (equal window jtk-window-mirror--current-pairing-master)
               (assoc window jtk-window-mirror--mirrors))
           "mirror(m)"
         (if (rassoc window jtk-window-mirror--mirrors)
             "mirror(s)"
           "")))))
  :global t
  (if (bound-and-true-p jtk-window-mirror-mode) 
      (add-hook 'post-command-hook 'jtk-window-mirror--mirror t)
    (remove-hook 'post-command-hook 'jtk-window-mirror--mirror)))

(provide 'jtk-window-mirror)

;;; jtk-window-mirror.el ends here
