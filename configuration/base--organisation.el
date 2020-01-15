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

  :config
  (setq
   org-gtd-directory "~/.gtd/"
   ;; org-gtd-todos-file (expand-file-name "todos.org" org-gtd-directory)
   ;; org-gtd-journal-file (expand-file-name "journal.org" org-gtd-directory)
   org-gtd-todos-file (expand-file-name "gtd.org" org-gtd-directory)
   org-gtd-journal-file (expand-file-name "gtd.org" org-gtd-directory))

  (setq
   org-startup-indented t
   org-M-RET-may-split-line t
   org-default-notes-file org-gtd-todos-file
   org-outline-path-complete-in-steps nil
   org-refile-use-outline-path t
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-agenda-files (list org-gtd-todos-file)

   org-agenda-prefix-format
   '((agenda . " %i %-25:(org-format-outline-path (cdr (org-get-outline-path)))%?-12t %s")
     (todo . " %i %-25:(org-format-outline-path (cdr (org-get-outline-path)))%-12:c")
     (tags . " %i %-25:(org-format-outline-path (cdr (org-get-outline-path)))%-12:c")
     (search . " %i %-25:(org-format-outline-path (cdr (org-get-outline-path)))%-12:c"))

   org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "BLOCKED(b)" "|" "DONE(d)" "DELEGATED(g)" "CANCELED(c)"))

   org-tag-alist
   '(;; For exclusiv groups
     ;; (:startgroup . nil)
     ;; ("@work" . ?w) ("@home" . ?h)
     ;; ("@tennisclub" . ?t)
     ;; (:endgroup . nil)
     ("GENERAL" . ?g) ("EMACS" . ?e))

   org-refile-targets '((org-agenda-files :maxlevel . 5)))

  (setq
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

  :bind*
  (("C-M-- <RET>" . org-capture)
   ("C-M-- t" . org-capture-todo)
   ("C-M-- T" . org-capture-todo-deadline)
   ("C-M-- j" . org-capture-journal)
   ("C-M-- c" . org-capture-calendar)
   ("C-M-- a" . org-agenda)))

(provide 'base--organisation)
