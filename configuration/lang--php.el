(use-package
  php-mode
  :mode
  ("\\.php\\'")

  :bind
  (:map php-mode-map
	("C-." . nil)))

(use-package
  geben
  :config
  (setq
   geben-pause-at-entry-line nil
   geben-show-breakpoints-debugging-only nil)
  :commands (geben))

(use-package
  company-php
  :requires flycheck
  :init
  ;; (defun company-ac-php-backend (command &optional arg &rest ignored)
  ;;   (interactive (list 'interactive))
  ;;   (cl-case command
  ;;     (interactive (company-begin-backend 'company-simple-backend))
  ;;     (prefix (when (eq major-mode 'php-mode)
  ;; 		(save-excursion
  ;; 		  (let* ((pos (point))
  ;; 			 (pos-m2 (- pos 2))
  ;; 			 (text (buffer-substring pos-m2 pos)))
  ;; 		    (if (string= text "->")
  ;; 			(progn
  ;; 			  (backward-char 2)
  ;; 			  (company-grab-symbol))
  ;; 		      (company-grab-symbol))))))
  ;;     (candidates (list (concat arg "_bar")))
  ;;     (meta (format "This value is named %s" arg))))
  
  (defun enable-company-php ()
    (interactive)
    (ac-php-core-eldoc-setup)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ac-php-backend))
  :hook (php-mode . enable-company-php)
  :bind*
  (:map php-mode-map
	("M-j" . ac-php-find-symbol-at-point)))

;; (use-package
;;   composer
;;   :commands composer)

(provide 'lang--php)
