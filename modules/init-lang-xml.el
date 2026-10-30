;;; init-lang-xml.el --- XML and XSLT -*- lexical-binding: t; -*-

;;; Commentary:

;; nxml-mode for XML, XSLT and XML Schema: validation while typing
;; against a schema (Emacs brings one for XSLT 1.0), completion of
;; elements and attributes, closing tags, moving by elements with the
;; keys for structural editing, folding elements (hideshow's C-c @
;; keys), and formatting.
;; lemminx, a language server installed outside Emacs, adds validation
;; and completion from XML Schema and DTDs through eglot.

;;; Code:

(defvar eglot-server-programs)

(defun jtk-xml-format ()
  "Indent the XML of the region, or the buffer, with xmllint.
The language server formats instead where one runs (M-r =)."
  (interactive)
  (let ((beginning (if (use-region-p) (region-beginning) (point-min)))
        (end (if (use-region-p) (region-end) (point-max)))
        (line (line-number-at-pos)))
    (unless (executable-find "xmllint")
      (user-error "xmllint not found"))
    (shell-command-on-region beginning end "xmllint --format -" nil t
                             "*xmllint errors*" t)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun jtk-xml-enable-folding ()
  "Turn on folding of elements with hideshow.
A block runs from an opening tag or comment to its end, found by moving
over the element."
  (setq-local hs-block-start-regexp "<!--\\|<[^/>!?][^>]*[^/]>"
              hs-block-end-regexp "-->\\|</[^>]*>"
              hs-c-start-regexp "<!--"
              hs-forward-sexp-function #'nxml-forward-element)
  (hs-minor-mode 1))

(use-package
  nxml-mode
  :ensure nil

  :mode
  ("\\.\\(xml\\|xsl\\|xslt\\|xsd\\)\\'" . nxml-mode)

  :custom
  (nxml-child-indent 2)
  (nxml-attribute-indent 2)
  ;; NOTE: Typing </ closes the innermost open element.
  (nxml-slash-auto-complete-flag t)

  :hook
  ((nxml-mode . jtk-xml-enable-folding)
   (nxml-mode . jtk-xml-start-language-server))

  :init
  (defun jtk-xml-start-language-server ()
    "Start lemminx through eglot, if it is installed."
    (when (executable-find "lemminx")
      (eglot-ensure)))

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nxml-mode . ("lemminx"))))

  :bind
  (:map nxml-mode-map
        ("C-<left>" . nxml-backward-element)
        ("C-<right>" . nxml-forward-element)
        ("C-<up>" . nxml-backward-up-element)
        ("C-<down>" . nxml-down-element)

        ("M-r =" . jtk-xml-format)
        ("C-;" . comment-dwim)))

(provide 'init-lang-xml)

;;; init-lang-xml.el ends here
