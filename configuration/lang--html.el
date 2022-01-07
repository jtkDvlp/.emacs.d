(use-package
  web-mode
  :mode
  "\\.\\(html\\|phtml\\|xml\\|xsl\\|xslt\\|xsd\\)\\'"

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-attr-value-indent-offset 2)

  (defun browse-current-file ()
    (interactive)
    (if (buffer-file-name)
        (browse-url
         (if (and (fboundp 'projectile-project-root)
                  (fboundp 'httpd-running-p))
             (let ((project-root-dir
                    (projectile-project-root))

                   (buffer-file-path
                    (buffer-file-name)))
               (if (and project-root-dir
                        (httpd-running-p))
                   (replace-regexp-in-string
                    (format "^%s" project-root-dir)
                    (format "http://%s:%d/"
                            (cl-case httpd-host
                              ((nil) "0.0.0.0")
                              ((local) "localhost")
                              (otherwise httpd-host))
                            httpd-port)
                    buffer-file-path)
                 (buffer-file-name)))
           (buffer-file-name)))
      (message "No file based buffer"))))

(use-package
  web-mode-edit-element
  :after web-mode
  :diminish web-mode-edit-element-minor-mode
  :hook (web-mode . web-mode-edit-element-minor-mode)

  :config
  (require 'html-mode-expansions)

  (defun web-mode-element-forward ()
    (interactive)
    (when (looking-at-p "[\s\n]+")
      (web-mode-tag-next))
    (web-mode-element-end))

  (defun web-mode-element-backward ()
    (interactive)
    (when (looking-at-p "[\s\n]+")
      (web-mode-tag-previous))
    (let ((curpos (point)))
      (web-mode-element-beginning)
      (when (= curpos (point))
	(web-mode-element-sibling-previous))))

  (defun web-mode-attribute-forward ()
    (interactive)
    (when (or (null (get-text-property (point) 'tag-attr))
	      (looking-at-p "[\s\n]+"))
      (web-mode-attribute-next))
    (web-mode-attribute-end))

  (defun web-mode-attribute-backward ()
    (interactive)
    (if (or (and (null (get-text-property (point) 'tag-attr))
		 (null (get-text-property (- (point) 1) 'tag-attr)))
	    (looking-at-p "[\s\n]+"))
	(web-mode-attribute-previous)
      (let ((curpos (point)))
	(web-mode-attribute-beginning)
	(when (= curpos (point))
	  (web-mode-attribute-previous)))))

  (defun web-mode-attribute-select-content ()
    (interactive)
    (web-mode-edit-element-attributes-end-inside)
    (call-interactively 'er/mark-inside-quotes))

  (defun web-mode-attribute-rename ()
    (interactive)
    (when (or (null (get-text-property (point) 'tag-attr))
	      (looking-at-p "[\s\n]+"))
      (web-mode-attribute-next))
    (web-mode-attribute-beginning)
    (call-interactively 'er/mark-word))

  (smartrep-define-key
      web-mode-edit-element-minor-mode-map
      "C-a"
    '(("C-<left>" . web-mode-attribute-backward)
      ("C-<right>" . web-mode-attribute-forward)

      ("C-M-<left>" . web-mode-edit-element-attributes-transpose-backward)
      ("C-M-<right>" . web-mode-attribute-transpose)

      ("C-(" . web-mode-attribute-insert)
      ("C-M-(" . web-mode-attribute-rename)

      ("C-k" . web-mode-attribute-kill)

      ("M-SPC" . web-mode-attribute-select-content)))

  :bind*
  (:map web-mode-edit-element-minor-mode-map
	("C-<left>" . web-mode-element-backward)
	("M-<left>" . web-mode-edit-element-elements-contract-over-border)
	;; ("C-M-<left>" . web-mode-element-beginning)

	("C-<right>" . web-mode-element-forward)
	("M-<right>" . web-mode-edit-element-elements-expand-over-border)
	;; ("C-M-<right>" . web-mode-element-end)

	("C-<up>" . web-mode-element-parent)
	("M-<up>" . web-mode-element-vanish)

	("C-<down>" . web-mode-element-child)
	("M-<down>" . web-mode-edit-element-elements-raise)

	("C-M-<up>" . web-mode-edit-element-elements-transpose-backward)
	("C-M-<down>" . web-mode-element-transpose)

	("C-(" . web-mode-element-wrap)
	("C-M-(" . web-mode-element-rename)

	("C-k" . web-mode-element-kill)
	("C-S-K" . web-mode-edit-element-elements-dissolve)

	("M-k" . web-mode-edit-element-elements-kill-siblings-next)
	("M-K" . web-mode-edit-element-elements-kill-siblings-previous)
	("C-M-k" . web-mode-edit-element-elements-kill-siblings)

	("M-SPC" . web-mode-element-content-select)

	("C-;" . web-mode-comment-or-uncomment)))

(provide 'lang--html)
