;;; check.el --- Check the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Run through `scripts/check', not directly.
;;
;; Loads `early-init.el' and `init.el' the way a normal start would,
;; runs the startup hooks, waits for pending package installs, then
;; byte-compiles every configuration file.  The result is a Markdown
;; report: whether loading worked, and per file the errors and warnings
;; the byte-compiler found.
;;
;; Exits non-zero when loading fails or a file does not compile.
;; Warnings alone do not fail the check; their count is the measure of
;; whether a change made things better or worse.

;;; Code:

(require 'backtrace)
(require 'bytecomp)
(require 'cl-lib)
(require 'subr-x)

(defconst check-root
  (file-name-directory
   (directory-file-name (file-name-directory load-file-name)))
  "Root directory of the configuration.")

(defconst check-source-globs
  '("early-init.el" "init.el" "configuration/*.el" "modules/*.el" "lisp/*.el")
  "Files to byte-compile, relative to `check-root'.")

(defvar check-load-error nil
  "The error that aborted loading, or nil.")

(defvar check-load-backtrace nil
  "Backtrace at the point `check-load-error' was signalled.")

(defconst check-backtrace-lines 25
  "How many lines of `check-load-backtrace' the report shows.
The innermost frames name the failing form; the rest is the loader.")

(defvar use-package-compute-statistics)

(defvar check-runtime-warnings nil
  "Warnings shown via `display-warning' while loading, newest first.")

(defun check--collect-runtime-warning (type message &optional level _buffer)
  "Remember a `display-warning' call with TYPE, MESSAGE and LEVEL."
  (push (list (or level :warning) type message) check-runtime-warnings))

(defun check--run-startup-hooks ()
  "Run the hooks a normal start runs after the init files.
NOTE: In batch mode `-l' loads the init files only after these hooks
already ran, so anything registered there (elpaca's queue, dashboard)
would otherwise never happen."
  (run-hooks 'after-init-hook 'emacs-startup-hook 'window-setup-hook))

(defun check--wait-for-packages ()
  "Block until elpaca has installed and configured everything queued."
  (when (fboundp 'elpaca-wait)
    (elpaca-wait)))

(defun check--load-init-files ()
  "Load the init files, finish startup and wait for packages."
  ;; WATCHOUT: Packages tell init from later by `after-init-time', which
  ;; batch mode has already set by now.  elpaca, for one, would treat
  ;; every order as made after init and leave it out of the lock file.
  ;; NOTE: The snapshot loads every declared package, and only
  ;; use-package's statistics know them all.
  (when (check--snapshot-file)
    (setq use-package-compute-statistics t))
  (let ((after-init-time nil))
    (dolist (file '("early-init.el" "init.el"))
      (let ((path (expand-file-name file check-root)))
        (when (file-exists-p path)
          (load path nil t)))))
  (check--run-startup-hooks)
  (check--wait-for-packages))

(defvar check-snapshot nil
  "What `check-take-snapshot' did, or nil if no snapshot was asked for.
A plist: `:file', `:lines' and `:unloadable'.")

(declare-function snapshot-load-declared-packages "snapshot")
(declare-function snapshot-write "snapshot")

(defun check--snapshot-file ()
  "Return the file CHECK_SNAPSHOT names, or nil."
  (let ((file (getenv "CHECK_SNAPSHOT")))
    (unless (or (null file) (string-empty-p file))
      file)))

(defun check-take-snapshot ()
  "Write the snapshot of `snapshot.el' if CHECK_SNAPSHOT names a file."
  (when-let* ((file (check--snapshot-file)))
    (load (expand-file-name "scripts/snapshot.el" check-root) nil t)
    (condition-case err
        (let* ((unloadable (snapshot-load-declared-packages))
               (lines (snapshot-write file unloadable)))
          (setq check-snapshot
                (list :file file :lines lines :unloadable unloadable)))
      (error (setq check-snapshot (list :file file :error err))))))

(defun check--remember-backtrace (_error)
  "Store the current backtrace in `check-load-backtrace'."
  (setq check-load-backtrace (backtrace-to-string)))

(defun check-load-configuration ()
  "Load the init files and finish startup; record a failure."
  (advice-add 'display-warning :before #'check--collect-runtime-warning)
  (condition-case err
      ;; NOTE: `handler-bind' sees the stack before `condition-case'
      ;; unwinds it; afterwards the failing form would be gone.
      (handler-bind ((error #'check--remember-backtrace))
        (check--load-init-files))
    (error (setq check-load-error err)))
  (advice-remove 'display-warning #'check--collect-runtime-warning))

(declare-function elpaca--queued "elpaca")
(declare-function elpaca<-status "elpaca")

(defun check-unfinished-packages ()
  "Return (ID . STATUS) for every elpaca order that did not finish."
  (when (fboundp 'elpaca--queued)
    (cl-loop for (id . order) in (elpaca--queued)
             for status = (elpaca<-status order)
             unless (eq status 'finished)
             collect (cons id status))))

(declare-function elpaca-write-lock-file "elpaca")

(defvar check-lock-change nil
  "How the lock file changed, or nil if it was not written.
A plist: `:before' and `:after' are alists of (ID . COMMIT); `:before'
is nil when there was no lock file yet.")

(defun check--locked-commits (file)
  "Return (ID . COMMIT) for every package pinned in lock FILE, or nil."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (mapcar (lambda (entry)
                (cons (car entry)
                      (plist-get (plist-get (cdr entry) :recipe) :ref)))
              (read (current-buffer))))))

(defun check-write-lock-file ()
  "Write `elpaca-lock-file' if CHECK_LOCK asks for it.
Records the commits before and after in `check-lock-change'."
  (when (and (getenv "CHECK_LOCK")
             (fboundp 'elpaca-write-lock-file)
             (boundp 'elpaca-lock-file))
    (let ((before (check--locked-commits elpaca-lock-file)))
      (make-directory (file-name-directory elpaca-lock-file) t)
      (elpaca-write-lock-file elpaca-lock-file)
      (setq check-lock-change
            (list :before before
                  :after (check--locked-commits elpaca-lock-file))))))

(defun check--lock-differences ()
  "Return the packages whose commit changed, as Markdown fragments."
  (let ((before (plist-get check-lock-change :before))
        (after (plist-get check-lock-change :after)))
    (append
     (cl-loop for (id . commit) in after
              for old = (alist-get id before)
              unless (equal old commit)
              collect (if old
                          (format "`%s` %.7s → %.7s" id old commit)
                        (format "`%s` new" id)))
     (cl-loop for (id . _) in before
              unless (assq id after)
              collect (format "`%s` gone" id)))))

(defun check-lock-mismatch-p ()
  "Return non-nil if CHECK_LOCK is `verify' and the lock file changed.
Without a lock file before there is nothing to verify."
  (and (equal (getenv "CHECK_LOCK") "verify")
       (plist-get check-lock-change :before)
       (check--lock-differences)))

(defun check--source-files ()
  "Return the existing files matching `check-source-globs'."
  (cl-loop for glob in check-source-globs
           append (file-expand-wildcards (expand-file-name glob check-root))))

(defun check--line-at (file position)
  "Return the line of POSITION in FILE, or nil if POSITION is not a number."
  (when (integerp position)
    (with-temp-buffer
      (insert-file-contents file)
      (line-number-at-pos (min position (point-max))))))

(defun check-compile-file (file output-directory)
  "Byte-compile FILE into OUTPUT-DIRECTORY.
Return a list of (LEVEL LINE MESSAGE), one per diagnostic."
  (let* ((diagnostics nil)
         (byte-compile-dest-file-function
          (lambda (source)
            (expand-file-name (concat (file-name-base source) ".elc")
                              output-directory)))
         (byte-compile-log-warning-function
          (lambda (message &optional position _fill level)
            (push (list (or level :warning)
                        (check--line-at file position)
                        message)
                  diagnostics)))
         (compiled (ignore-errors (byte-compile-file file))))
    (when (and (null compiled)
               (not (cl-find :error diagnostics :key #'car)))
      (push (list :error nil "byte-compile-file failed") diagnostics))
    (nreverse diagnostics)))

(defun check-compile-all ()
  "Byte-compile all source files.
Return an alist of (FILE . DIAGNOSTICS)."
  (let ((output-directory (make-temp-file "check-elc" t)))
    (unwind-protect
        (mapcar (lambda (file)
                  (cons file (check-compile-file file output-directory)))
                (check--source-files))
      (delete-directory output-directory t))))

(defun check--count (level diagnostics)
  "Count the DIAGNOSTICS of LEVEL; `:warning' also counts unknown levels."
  (cl-count-if (lambda (diagnostic)
                 (if (eq level :error)
                     (eq (car diagnostic) :error)
                   (not (eq (car diagnostic) :error))))
               diagnostics))

(defun check--one-line (message)
  "Return MESSAGE as a single trimmed line."
  (string-trim (replace-regexp-in-string "\n+" " " (format "%s" message))))

(defun check--relative (file)
  "Return FILE relative to `check-root'."
  (file-relative-name file check-root))

(defconst check-backtrace-line-width 160
  "Frames are cut to this many characters; some print whole forms.")

(defun check--backtrace-frames ()
  "Return the frames of `check-load-backtrace' below the handler itself."
  (thread-last (split-string check-load-backtrace "\n" t)
               (seq-drop-while (lambda (line)
                                 (not (string-search
                                       "check--remember-backtrace" line))))
               (cdr)
               (mapcar (lambda (line)
                         (truncate-string-to-width
                          line check-backtrace-line-width nil nil "…")))))

(defun check--report-backtrace ()
  "Return the start of `check-load-backtrace' as a collapsed block."
  (let ((lines (seq-take (check--backtrace-frames) check-backtrace-lines)))
    (concat "<details><summary>Backtrace</summary>\n\n```\n"
            (string-join lines "\n")
            "\n```\n\n</details>\n\n")))

(defun check--report-packages (unfinished)
  "Return the report line on packages; UNFINISHED as from elpaca."
  (if (null unfinished)
      "**Packages:** all installed\n\n"
    (concat "**Packages:** not installed — "
            (mapconcat (pcase-lambda (`(,id . ,status))
                         (format "`%s` (%s)" id status))
                       unfinished ", ")
            "\n\n")))

(defun check--report-lock ()
  "Return the report line on the lock file, if it was written."
  (when check-lock-change
    (let ((differences (check--lock-differences))
          (count (length (plist-get check-lock-change :after))))
      (cond
       ((null (plist-get check-lock-change :before))
        (format "**Lock file:** written for %d packages, none before\n\n"
                count))
       ((null differences)
        (format "**Lock file:** reproduced, %d packages at their commits\n\n"
                count))
       (t
        (concat "**Lock file:** changed — "
                (string-join differences ", ")
                "\n\n"))))))

(defun check--report-snapshot ()
  "Return the report line on the snapshot, if one was taken."
  (cond
   ((plist-get check-snapshot :error)
    (format "**Snapshot:** failed — `%S`\n\n"
            (plist-get check-snapshot :error)))
   ((plist-get check-snapshot :lines)
    (concat (format "**Snapshot:** %d lines" (plist-get check-snapshot :lines))
            (when-let* ((unloadable (plist-get check-snapshot :unloadable)))
              (format "; declared but not loadable as a feature: %s"
                      (mapconcat (lambda (name) (format "`%s`" name))
                                 unloadable ", ")))
            "\n\n"))))

(defun check--report-load ()
  "Return the report section on loading."
  (concat
   (if check-load-error
       (concat (format "**Load:** failed — `%S`\n\n" check-load-error)
               (when check-load-backtrace
                 (check--report-backtrace)))
     "**Load:** ok\n\n")
   (when check-runtime-warnings
     (concat
      "Warnings shown while loading:\n\n"
      (mapconcat (lambda (warning)
                   (pcase-let ((`(,level ,type ,message) warning))
                     (format "- %s `%S`: %s" level type
                             (check--one-line message))))
                 (reverse check-runtime-warnings)
                 "\n")
      "\n\n"))))

(defun check--report-table (results)
  "Return the per-file table for compile RESULTS."
  (let ((total-errors 0)
        (total-warnings 0))
    (concat
     "| File | Compile errors (count) | Compile warnings (count) |\n"
     "|---|---:|---:|\n"
     (mapconcat (lambda (result)
                  (let ((errors (check--count :error (cdr result)))
                        (warnings (check--count :warning (cdr result))))
                    (cl-incf total-errors errors)
                    (cl-incf total-warnings warnings)
                    (format "| `%s` | %d | %d |"
                            (check--relative (car result)) errors warnings)))
                results
                "\n")
     (format "\n| **Total** | **%d** | **%d** |\n\n"
             total-errors total-warnings))))

(defun check--report-details (results)
  "Return every compile diagnostic in RESULTS as a Markdown list."
  (concat
   "<details><summary>All compile diagnostics</summary>\n\n"
   (mapconcat
    (lambda (result)
      (mapconcat (pcase-lambda (`(,level ,line ,message))
                   (format "- %s `%s:%s` %s"
                           level (check--relative (car result)) (or line "?")
                           (check--one-line message)))
                 (cdr result)
                 "\n"))
    (cl-remove-if-not #'cdr results)
    "\n")
   "\n\n</details>\n"))

(defun check-report (unfinished results)
  "Return the full Markdown report.
UNFINISHED lists packages that did not install, RESULTS the compile
diagnostics; RESULTS is nil when loading failed and nothing was
compiled."
  (concat
   (format "## Configuration check\n\nGNU Emacs %s\n\n" emacs-version)
   (check--report-load)
   (check--report-packages unfinished)
   (check--report-lock)
   (check--report-snapshot)
   (if results
       (concat (check--report-table results)
               (check--report-details results))
     "Byte-compile skipped: without a loaded configuration its packages
are missing, and every file would only report them.\n")))

(defun check-failed-p (unfinished results)
  "Return non-nil if the check failed.
That is: loading failed, a package in UNFINISHED did not install, a
verified lock file changed, or a file in RESULTS has compile errors."
  (or check-load-error
      unfinished
      (check-lock-mismatch-p)
      (cl-some (lambda (result) (< 0 (check--count :error (cdr result))))
               results)))

(defun check--write-report (report)
  "Print REPORT and append it to the file named by CHECK_REPORT, if any."
  (let ((report-file (getenv "CHECK_REPORT")))
    (princ report)
    (when (and report-file (not (string-empty-p report-file)))
      (write-region report nil report-file t))))

(defun check-main ()
  "Run the check, print the report and exit with its status."
  (check-load-configuration)
  (let ((unfinished (check-unfinished-packages)))
    ;; NOTE: Only a complete install is worth pinning.
    (unless (or check-load-error unfinished)
      (check-write-lock-file)
      ;; NOTE: Before compiling, which loads files of its own.
      (check-take-snapshot))
    (let ((results (unless check-load-error
                     (check-compile-all))))
      (check--write-report (check-report unfinished results))
      (kill-emacs (if (check-failed-p unfinished results) 1 0)))))

(check-main)

;;; check.el ends here
