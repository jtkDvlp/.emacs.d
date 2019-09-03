(defun ide/main-window ()
  (let* ((most-top-left-window
	  (window-at 0 0))
	 
	 (treemacs-buffer
	  (when (bound-and-true-p treemacs--buffer-access)
	    (assoc-default (selected-frame) treemacs--buffer-access)))

	 (treemacs-window
	  (when treemacs-buffer
	    (get-buffer-window treemacs-buffer))))
    
    (with-selected-window most-top-left-window
      (if (eq most-top-left-window treemacs-window)
	  (window-in-direction 'right nil t 1)
	most-top-left-window))))

(defun ide/last-window-in-direction (direction &optional window)
  (let ((window
	 (or window (selected-window))))
    
    (while (window-in-direction direction window t 1)
      (setq window (window-in-direction direction window t 1)))

    window))

(defun ide/nth-ide-window-in-direction (nth direction &optional window)
  (let ((origin-window
	 (or window (selected-window)))

	(index -1))

    (while (and origin-window (< index nth))
      (let ((is-ide-window
	     (window-parameter origin-window 'ide)))

	(when is-ide-window
	  (setq index (+ 1 index)))

	(unless (= index nth)
	  (setq origin-window
		(window-in-direction direction origin-window t 1)))))
    
    (when (= index nth)
      origin-window)))

(defun ide/display-buffer-in-ide-window (buffer options) 
  (let* ((main-window
	  (ide/main-window))
	 
	 (orientation
	  (or (cdr (assoc 'orientation options)) 'vertical))

	 (direction
	  (if (eq orientation 'vertical) 'right 'below))

	 (slot
	  (cdr (assoc 'slot options)))
	 
	 (size
	  (- (if (eq orientation 'vertical) (window-width main-window t) (window-height main-window t))
	     (cdr (assoc 'size options))))
	 
	 (last-window-on-side
	  (ide/last-window-in-direction direction main-window))
         
	 (ide-window
	  (if (eq main-window last-window-on-side)
	      (split-window main-window size direction t)

	    (let* ((direction
		    (if (eq orientation 'horizontal) 'right 'below))

		   (ide-window-at-slot
		    (ide/nth-ide-window-in-direction
		     slot direction last-window-on-side)))
	      
	      (if ide-window-at-slot
		  ide-window-at-slot
		(split-window
		 last-window-on-side
		 nil
		 direction
		 t)))))) 

    (mapc
     (lambda (pair)
       (set-window-parameter ide-window (car pair) (cdr pair)))
     (cdr (assoc 'window-parameters options)))

    (set-window-parameter ide-window 'ide t)
    
    (with-selected-window ide-window 
      (switch-to-buffer buffer t t))

    ide-window))

(defun ide/configure-frame-layout (layout &optional options) 
  (let* ((window-parameters
	  `(window-parameters . ((no-other-window . t)
				 (no-delete-other-windows . t))))

	 (horizontal-layout (car layout))
	 (horizontal-size (or (cdr (assoc 'horizontal-size options)) 300))

	 (horizontal-i -1)

	 (horizontal-display-buffers
	  (mapcar (lambda (buffer-name-or-regex)
		    (setq horizontal-i (+ 1 horizontal-i))
		    `(,buffer-name-or-regex
		      ide/display-buffer-in-ide-window
		      (orientation . horizontal)
		      (slot . ,horizontal-i)
		      (size . ,horizontal-size)
		      ,window-parameters))
		  horizontal-layout))

	 (vertical-layout (car (cdr layout)))
	 (vertical-size (or (cdr (assoc 'vertical-size options)) 500))

	 (vertical-i -1)
	 
	 (vertical-display-buffers
	  (mapcar (lambda (buffer-name-or-regex)
		    (setq vertical-i (+ 1 vertical-i))
		    `(,buffer-name-or-regex
		      ide/display-buffer-in-ide-window
		      (orientation . vertical)
		      (slot . ,vertical-i)
		      (size . ,vertical-size)
		      ,window-parameters))
		  vertical-layout)))

    (setq fit-window-to-buffer-horizontally t
	  window-resize-pixelwise t
	  display-buffer-alist `(,@horizontal-display-buffers ,@vertical-display-buffers))))

(defun ide/open-buffers (buffers-and-commands) 
  (let ((current-window (selected-window)))
    (mapc
     (lambda (buffer-or-command)
       (if (commandp buffer-or-command t)
	   (call-interactively buffer-or-command) 
	 (switch-to-buffer-other-window buffer-or-command t)))
     buffers-and-commands)
    (select-window current-window)))

(defun ide/enable-layout (layout defaults)
  (ide/configure-frame-layout layout) 
  (ide/open-buffers defaults))

(defvar ide/default-layout
  `(("\\*\\(?:eshell\s[^\\*]*\\|compilation\\)\\*")
    ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
     "\\*Flycheck error messages\\*")))

(defvar ide/default-buffers
  `("*Help*" projectile-run-eshell treemacs))

(provide 'pkg--ide)
