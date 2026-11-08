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
(defvar eshell-last-output-end)

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

(defun jtk-babashka-eshell-placeholder (&rest _)
  "Stand in as `eshell/bbe' and `eshell/clje', which eshell never calls.
The commands run through `jtk-babashka-eshell-command' in
`eshell-named-command-hook', which comes first; a function by their
name makes them known commands, for highlighting and `which'."
  (error "Run through `jtk-babashka-eshell-command' in %s"
         'eshell-named-command-hook))

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

(defun jtk-babashka--quoted-p (position input-start)
  "Return non-nil if POSITION lies within quotes of the eshell input.
INPUT-START is where the input begins.  In single quotes, two quotes in
a row are a quote; in double quotes, a backslash escapes the next
character."
  (let ((quote nil))
    (save-excursion
      (goto-char input-start)
      (while (< (point) position)
        (let ((char (char-after)))
          (cond ((and (eq quote ?\") (eq char ?\\)) (forward-char))
                ((eq char quote) (setq quote nil))
                ((and (null quote) (memq char '(?' ?\"))) (setq quote char))))
        (forward-char)))
    quote))

(defun jtk-babashka--command-position-p (position input-start)
  "Return non-nil if a command can start at POSITION.
That is at INPUT-START, the start of the eshell input, or after a pipe,
`;' or `&', with only blanks in between, and not within quotes."
  (and (not (jtk-babashka--quoted-p position input-start))
       (save-excursion
         (goto-char position)
         (skip-chars-backward " \t" input-start)
         (or (= (point) input-start)
             (memq (char-before) '(?| ?\; ?&))))))

(defun jtk-babashka--form-end (start)
  "Return where the form starting at START ends.
A single-quoted form ends at its closing quote; an unquoted or open one
at the next pipe, `;' or `&', or at the end of the line."
  (save-excursion
    (goto-char start)
    (if (looking-at "'\\(?:[^']\\|''\\)*'")
        (match-end 0)
      (skip-chars-forward "^|;&\n")
      (skip-chars-backward " \t" start)
      (point))))

(defun jtk-babashka--input-bounds ()
  "Return the bounds of the eshell input at point, as (START . END).
The input being typed runs from the prompt to the end of the buffer,
over several lines if a form has them; earlier input counts by line."
  (if (and (markerp (bound-and-true-p eshell-last-output-end))
           (>= (point) eshell-last-output-end))
      (cons (marker-position eshell-last-output-end) (point-max))
    (cons (save-excursion (beginning-of-line) (point))
          (line-end-position))))

(defun jtk-babashka--commands-on-line ()
  "Return our commands in the eshell input at point, from left to right.
Each is (COMMAND-START FORM-START FORM-END), wherever a command can
start: at the beginning, or after a pipe, `;' or `&'."
  (save-excursion
    (pcase-let* ((`(,input-start . ,input-end) (jtk-babashka--input-bounds))
                 (regexp (concat (regexp-opt jtk-babashka-eshell-commands
                                             'symbols)
                                 "[ \t]*"))
                 (commands nil))
      (goto-char input-start)
      (while (re-search-forward regexp input-end t)
        (when (jtk-babashka--command-position-p (match-beginning 0)
                                                input-start)
          (push (list (match-beginning 0) (match-end 0)
                      (jtk-babashka--form-end (match-end 0)))
                commands)))
      (nreverse commands))))

(defun jtk-babashka--command-at-point ()
  "Return the command of `jtk-babashka--commands-on-line' point is in.
That is the one whose text holds point, else the last one before point,
else the first; nil if the input runs none of ours."
  (let ((point (point))
        (commands (jtk-babashka--commands-on-line)))
    (or (seq-find (pcase-lambda (`(,start ,_ ,end))
                    (<= start point end))
                  commands)
        (car (last (seq-filter (pcase-lambda (`(,start . ,_))
                                 (<= start point))
                               commands)))
        (car commands))))

(defun jtk-babashka-eshell-command-line-p ()
  "Return non-nil if the eshell input at point runs one of our commands.
Also inside a pipe or after `;', as in `cat file | bbe ...'."
  (and (jtk-babashka--command-at-point) t))

(defun jtk-babashka--in-eshell-command-p ()
  "Return non-nil if point is in the form of one of our eshell commands."
  (pcase (jtk-babashka--command-at-point)
    (`(,_ ,form-start ,form-end) (<= form-start (point) form-end))))

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
  "Return the bounds of the form of our command at point, as (START . END).
The form is the command's first argument: a single-quoted string, or
the rest up to a pipe or the end of the line when there is none yet."
  (pcase (jtk-babashka--command-at-point)
    (`(,_ ,start ,end) (cons start end))
    (_ (user-error "No %s command here"
                   (string-join jtk-babashka-eshell-commands " or ")))))

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
