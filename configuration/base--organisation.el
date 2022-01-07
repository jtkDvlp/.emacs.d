(use-package
  org

  :mode
  ("\\.org\\'" . org-mode)

  :commands
  (org-agenda
   org-capture-todo
   org-capture-todo-context
   org-capture-journal
   org-capture-calendar)

  :init
  (setq-default
   org-gtd-directory "~/.gtd/"
   ;; org-gtd-todos-file (expand-file-name "todos.org" org-gtd-directory)
   ;; org-gtd-journal-file (expand-file-name "journal.org" org-gtd-directory)
   org-gtd-todos-file (expand-file-name "gtd.org" org-gtd-directory)
   org-gtd-journal-file (expand-file-name "gtd.org" org-gtd-directory)

   org-cycle-include-plain-lists 'integrate
   org-startup-indented t
   org-startup-folded 'content
   org-M-RET-may-split-line nil
   org-default-notes-file org-gtd-todos-file
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-agenda-files (list org-gtd-todos-file)
   org-ellipsis "--more"
   org-default-priority 65

   org-agenda-prefix-format
   '((agenda . " %i %-30:(org-format-outline-path (org-get-outline-path))%?-12t %s")
     (todo . " %i %-30:(org-format-outline-path (org-get-outline-path))")
     (tags . " %i %-30:(org-format-outline-path (org-get-outline-path))")
     (search . " %i %-30:(org-format-outline-path (org-get-outline-path))"))

   org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)"))

   org-tag-alist
   '(;; For exclusiv groups
     ;; (:startgroup . nil)
     ;; ("@work" . ?w) ("@home" . ?h)
     ;; ("@tennisclub" . ?t)
     ;; (:endgroup . nil)
     ("GENERAL" . ?g) ("EMACS" . ?e) ("DAILY" . ?d)
     ("RETRO" . ?r))

   org-refile-targets '((org-agenda-files :maxlevel . 4))

   org-capture-templates
   '(("t" "Todo (without deadline)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("T" "Todo (with deadline)" entry (file+headline org-gtd-todos-file "Inbox")
      "* TODO %? %i %^g\nDEADLINE: %^t\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("j" "Journal" entry (file+headline org-gtd-journal-file "Journal")
      "* %? %i %^g\n:PROPERTIES:\n:ADDED: %U\n:END:")
     ("c" "Calendar" entry (file+headline org-gtd-todos-file "Calendar")
      "* TODO %? %i %^g\nSCHEDULED: %^T\n:PROPERTIES:\n:ADDED: %U\n:END:")))

  (defun org-capture-todo ()
    (interactive)
    (org-capture nil "t"))

  (defun org-capture-todo-deadline ()
    (interactive)
    (org-capture nil "T"))

  (defun org-capture-journal ()
    (interactive)
    (org-capture nil "j"))

  (defun org-capture-calendar ()
    (interactive)
    (org-capture nil "c"))

  (defun org-insert-jira-issue (&optional jira-issue)
    (interactive "MJIRA Issue: ")
    (let* ((jira-issue
            (upcase jira-issue))

           (jira-issue-link
            (concat env-jira-host-url "browse/" jira-issue)))

      (org-insert-link nil jira-issue-link jira-issue)))

  (defun org-backward-element-with-beginning ()
    (interactive)
    (let ((cursor (point)))
      (org-beginning-of-line)
      (when (= cursor (point))
        (org-backward-element))))

  (defun org-forward-element-with-end ()
    (interactive)
    (let ((cursor (point)))
      (org-end-of-line)
      (when (= cursor (point))
        (org-forward-element)
        (org-end-of-line))))

  (defun org-kill-list-item ()
    (interactive)
    (beginning-of-line)
    (org-kill-line)
    (org-kill-line))

  (defun org-kill-item ()
    (interactive)
    (let ((e (save-excursion (beginning-of-line) (org-element-at-point))))
      (if (memq (org-element-type e) '(item plain-list))
          (org-kill-list-item)
        (org-cut-special))))

  :bind*
  (("C-M-- <RET>" . org-capture)
   ("C-M-- t" . org-capture-todo)
   ("C-M-- T" . org-capture-todo-deadline)
   ("C-M-- j" . org-capture-journal)
   ("C-M-- c" . org-capture-calendar)
   ("C-M-- a" . org-agenda)

   ("C--" . org-narrow-to-subtree)

   :map org-mode-map
   ("M-i l" . org-insert-link)
   ("M-i j" . org-insert-jira-issue)
   ("M-i d" . org-deadline)

   ("M-j j" . org-open-at-point)

   ("C-<up>" . org-up-element)
   ("C-<down>" . org-down-element)
   ("C-<left>" . org-backward-element-with-beginning)
   ("C-<right>" . org-forward-element-with-end)

   ("C-M-<up>" . org-drag-element-backward)
   ("C-M-<down>" . org-drag-element-forward)

   ("C-k" . org-kill-line)
   ("C-S-k" . org-kill-item)))

(provide 'base--organisation)
