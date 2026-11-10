;;; init-lang-org.el --- Org -*- lexical-binding: t; -*-

;;; Commentary:

;; Org for structured notes and structured to-do lists: headings and
;; lists fold with TAB, the keys for structural editing move along and
;; rearrange headings and list items as they do s-expressions in code,
;; and C-- narrows to the subtree.  No agenda, capturing or GTD files.

;;; Code:

(declare-function org-narrow-to-subtree "org")

(defun jtk-org-narrow-dwim (beginning end)
  "Narrow to the region from BEGINNING to END if active, else the subtree.
As `jtk-narrow-dwim' does elsewhere, with the subtree for the defun."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (use-region-p)
      (narrow-to-region beginning end)
    (org-narrow-to-subtree)))

(use-package
  org
  :ensure nil

  :custom
  (org-startup-indented t)
  (org-startup-folded 'content)
  ;; NOTE: Plain lists fold like headings.
  (org-cycle-include-plain-lists 'integrate)
  (org-M-RET-may-split-line nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-ellipsis "--more")
  (org-todo-keywords
   '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)"
               "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)")))

  :bind
  (:map org-mode-map
        ("C--" . jtk-org-narrow-dwim)
        ("M-j j" . org-open-at-point))

  :config
  ;; NOTE: The keys for structural editing, with Org's own commands:
  ;; C- moves along and between levels, M- changes a heading or item
  ;; (promote, demote, move), C-M- the same for the whole subtree or
  ;; drags the element; C-k kills the line, C-S-k the subtree.
  (jtk-structural-editing-bind
   org-mode-map
   '(("C-<left>" . org-backward-element)
     ("C-<right>" . org-forward-element)
     ("C-<up>" . org-up-element)
     ("C-<down>" . org-down-element)

     ("M-<left>" . org-metaleft)
     ("M-<right>" . org-metaright)
     ("M-<up>" . org-metaup)
     ("M-<down>" . org-metadown)

     ("C-M-<left>" . org-shiftmetaleft)
     ("C-M-<right>" . org-shiftmetaright)
     ("C-M-<up>" . org-drag-element-backward)
     ("C-M-<down>" . org-drag-element-forward)

     ("C-k" . org-kill-line)
     ("C-S-K" . org-cut-subtree))))

(provide 'init-lang-org)

;;; init-lang-org.el ends here
