;;; jtk-babashka.el --- Babashka from Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Clojure through Babashka (bb) without a project: as commands in
;; eshell (bbe, clje), with completion and a Clojure buffer to edit the
;; form in; the tasks of a project's bb.edn; and a scratch REPL.  bb runs
;; on this machine, also where eshell's directory is on a remote host.

;;; Code:

(require 'subr-x)

(declare-function eshell-remote-command "esh-ext")
(declare-function cider-current-repl "cider-connection")
(declare-function cider-jack-in-clj "cider")
(declare-function clojure-mode "clojure-mode")
(defvar eshell-in-pipeline-p)

;; NOTE: Not `clj', which would hide the Clojure CLI's `clj' in eshell.
(defvar jtk-babashka-eshell-commands '("bbe" "clje")
  "Eshell commands that evaluate Clojure with Babashka.")

(defvar jtk-babashka-scratch-file nil
  "File of the scratch buffer for `jtk-babashka-scratch'.")

;;;; bb on this machine

(defun jtk-babashka-executable ()
  "Return the local bb, also when the current directory is remote."
  (let ((default-directory (expand-file-name "~/")))
    (or (executable-find "bb")
        (user-error "Babashka (bb) not found"))))

;;;; In eshell

(defun jtk-babashka--fetch (args)
  "Return ARGS with remote files replaced by local copies, and the copies.
The value is (ARGS . COPIES).  Only arguments that name a regular file
relative to a remote current directory are copied."
  (let (copies)
    (cons (mapcar (lambda (arg)
                    (let ((file (and (stringp arg) (expand-file-name arg))))
                      (if (and file (file-remote-p file) (file-regular-p file))
                          (car (push (file-local-copy file) copies))
                        arg)))
                  args)
          copies)))

(defun jtk-babashka--delete-when-done (process files)
  "Delete FILES once PROCESS has ended."
  (when (and files (processp process))
    (add-function :after (process-sentinel process)
                  (lambda (proc _event)
                    (unless (process-live-p proc)
                      (mapc (lambda (file) (ignore-errors (delete-file file)))
                            files))))))

(defun jtk-babashka-eshell-command (command args)
  "Evaluate the form in ARGS with bb if COMMAND is one of ours.
The first of ARGS is the form; input from a pipe is `*input*', a
sequence of lines; a sequence as result prints line by line.  The
other ARGS are `*command-line-args*', where a file on a remote current
directory becomes a local copy.  For `eshell-named-command-hook'."
  (when (member command jtk-babashka-eshell-commands)
    (pcase-let* ((piped? (memq eshell-in-pipeline-p '(t last)))
                 (`(,files . ,copies) (jtk-babashka--fetch (cdr args)))
                 (process
                  (eshell-remote-command
                   (jtk-babashka-executable)
                   (append (list (if piped? "-io" "-o") "-e" (car args))
                           files))))
      (jtk-babashka--delete-when-done process copies)
      ;; WATCHOUT: Return the process.  With only t, eshell cannot
      ;; connect a pipe to it, and the input shows on screen instead.
      (or process t))))

;;;; Completion

(defvar jtk-babashka--symbols nil
  "Names bb knows in its user namespace, with aliases, once asked.")

(defconst jtk-babashka--symbols-form
  "(let [core (map str (keys (ns-publics 'clojure.core)))
         aliased (for [[a n] (ns-aliases *ns*)
                       s (keys (ns-publics n))]
                   (str a \"/\" s))]
     (run! println (concat core aliased)))"
  "Clojure that prints every name the forms of `bbe' can use.")

(defun jtk-babashka-symbols ()
  "Return the names bb knows, asking bb on the first call."
  (or jtk-babashka--symbols
      (setq jtk-babashka--symbols
            (let ((default-directory (expand-file-name "~/")))
              (process-lines (jtk-babashka-executable)
                             "-e" jtk-babashka--symbols-form)))))

(defun jtk-babashka-eshell-command-line-p ()
  "Return non-nil if the eshell input at point runs one of our commands.
The match data then hold the command's name."
  (save-excursion
    (beginning-of-line)
    (looking-at (regexp-opt jtk-babashka-eshell-commands 'words))))

(defun jtk-babashka--in-eshell-command-p ()
  "Return non-nil if point is in the input of one of our eshell commands."
  (let ((point (point)))
    (and (jtk-babashka-eshell-command-line-p)
         (<= (match-end 0) point))))

(defun jtk-babashka-completion-at-point ()
  "Complete a Clojure name in the form of `bbe' or `clje' in eshell."
  (when (jtk-babashka--in-eshell-command-p)
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic
             (lambda (_) (jtk-babashka-symbols)))
            :exclusive 'no))))

;;;; Editing the form in a Clojure buffer

(defvar-local jtk-babashka--edit-target nil
  "Where the edited form goes back to, as (BUFFER START END).")

(defun jtk-babashka--quote (form)
  "Return FORM single-quoted for eshell."
  (concat "'" (string-replace "'" "''" form) "'"))

(defun jtk-babashka--unquote (text)
  "Return TEXT without eshell's single quotes, or TEXT."
  (if (string-match "\\`'\\(\\(?:[^']\\|''\\)*\\)'\\'" text)
      (string-replace "''" "'" (match-string 1 text))
    text))

(defun jtk-babashka--form-bounds ()
  "Return the bounds of the form after our command on this input line.
The form is the first argument: a single-quoted string, or the rest of
the line when there is none yet."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at (concat (regexp-opt jtk-babashka-eshell-commands
                                            'words)
                                " *"))
      (user-error "Not a %s command"
                  (string-join jtk-babashka-eshell-commands " or ")))
    (goto-char (match-end 0))
    (let ((start (point)))
      (if (looking-at "'\\(?:[^']\\|''\\)*'")
          (cons start (match-end 0))
        (cons start (line-end-position))))))

(defun jtk-babashka-edit-form ()
  "Edit the form of the `bbe' command at point in a Clojure buffer.
\\<jtk-babashka-edit-mode-map>\\[jtk-babashka-edit-finish] puts it \
back quoted, \\[jtk-babashka-edit-abort] drops the change."
  (interactive)
  (pcase-let* ((`(,start . ,end) (jtk-babashka--form-bounds))
               (form (jtk-babashka--unquote
                      (buffer-substring-no-properties start end)))
               (target (list (current-buffer)
                             (copy-marker start) (copy-marker end t))))
    (pop-to-buffer (generate-new-buffer "*bbe form*"))
    (insert form)
    (goto-char (point-min))
    (clojure-mode)
    (jtk-babashka-edit-mode 1)
    (setq jtk-babashka--edit-target target)))

(defun jtk-babashka-edit-finish ()
  "Put the edited form back into eshell, quoted."
  (interactive)
  (pcase-let ((`(,buffer ,start ,end) jtk-babashka--edit-target)
              (form (string-trim (buffer-string))))
    (quit-window 'kill)
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert (jtk-babashka--quote form))))
    (pop-to-buffer buffer)))

(defun jtk-babashka-edit-abort ()
  "Drop the edited form."
  (interactive)
  (quit-window 'kill))

(defvar-keymap jtk-babashka-edit-mode-map
  "C-o" #'jtk-babashka-edit-finish
  "C-c C-c" #'jtk-babashka-edit-finish
  "C-c C-k" #'jtk-babashka-edit-abort)

(define-minor-mode jtk-babashka-edit-mode
  "Editing the form of a `bbe' command.
\\{jtk-babashka-edit-mode-map}"
  :lighter " bbe")

;;;; Tasks of bb.edn

(defun jtk-babashka-tasks ()
  "Return the names of the tasks in the bb.edn of the current directory.
bb reads them itself, since bb.edn may use reader syntax beyond EDN."
  (with-temp-buffer
    (when (zerop (process-file "bb" nil t nil "tasks"))
      (goto-char (point-min))
      (let (tasks)
        ;; NOTE: `bb tasks' prints a header line, a blank line, then one
        ;; task per line with its docstring after the name.
        (forward-line 2)
        (while (re-search-forward "^\\([^ \n]+\\)" nil t)
          (push (match-string 1) tasks))
        (nreverse tasks)))))

;;;; Scratch REPL

(defun jtk-babashka-scratch ()
  "Show a Clojure scratch buffer with a Babashka REPL.
Starts the REPL once, in the scratch file's directory, outside any
other project."
  (interactive)
  (let* ((dir (file-name-directory jtk-babashka-scratch-file))
         (project-file (expand-file-name "bb.edn" dir)))
    (make-directory dir t)
    ;; NOTE: An empty bb.edn makes the directory a Babashka project, so
    ;; CIDER jacks in there without asking for a directory or a tool.
    (unless (file-exists-p project-file)
      (write-region "{}\n" nil project-file))
    (find-file jtk-babashka-scratch-file)
    (require 'cider)
    (unless (cider-current-repl nil)
      (cider-jack-in-clj '(:project-type babashka)))))

(provide 'jtk-babashka)

;;; jtk-babashka.el ends here
