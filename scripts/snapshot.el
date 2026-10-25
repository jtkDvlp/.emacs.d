;;; snapshot.el --- What the config set -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by check.el when `scripts/check --snapshot FILE' asks for it.
;;
;; After the configuration has loaded, every package it declares is
;; loaded as well, so that deferred keymaps, options and hooks exist.
;; Then one sorted line is written per key binding, user option, hook
;; entry and `auto-mode-alist' entry.  Diffing the snapshots of two
;; commits shows what a change did to the behaviour of Emacs, not to its
;; code: a pure restructuring has an empty diff.
;;
;; Not covered: anything only a running GUI or an interactive command
;; shows, such as faces, the mode line or window layouts.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar use-package-statistics)

(defconst snapshot-keymap-depth 6
  "How deep prefix keymaps are followed.
Deeper than any binding of the configuration; it only stops cycles.")

(defconst snapshot-key-range-limit 255
  "Ranges of at most this many characters are listed key by key.
Larger ones, such as the self-inserting characters, stay one line.")

(defun snapshot--declared-packages ()
  "Return the names of all `use-package' declarations, sorted."
  (let (names)
    (when (boundp 'use-package-statistics)
      (maphash (lambda (name _) (push name names)) use-package-statistics))
    (sort names #'string<)))

(defun snapshot-load-declared-packages ()
  "Load every declared package; return the names that failed to load.
Names that are not features (`emacs', `c++-mode') count as failed;
their bindings are covered through the features that define them."
  (cl-loop for name in (snapshot--declared-packages)
           unless (ignore-errors (require name) t)
           collect name))

(defun snapshot--shorten-paths (text)
  "Replace the varying parts of file names in TEXT by stable ones.
The check runs with a temporary HOME and from varying checkouts."
  (let ((root (abbreviate-file-name
               (directory-file-name (expand-file-name user-emacs-directory)))))
    (thread-last text
                 (string-replace (directory-file-name user-emacs-directory)
                                 "<root>")
                 (string-replace root "<root>")
                 (string-replace (expand-file-name "~") "~")
                 ;; Names made by `make-temp-file', e.g. org-persist's.
                 (replace-regexp-in-string
                  (concat (regexp-quote (directory-file-name
                                         temporary-file-directory))
                          "/[[:alnum:]._-]+")
                  "<tmp>"))))

(defun snapshot--function (function)
  "Return a stable printed form of FUNCTION."
  (cond ((symbolp function) (symbol-name function))
        ((interpreted-function-p function)
         (format "(lambda %S %S)" (aref function 0) (aref function 1)))
        ((byte-code-function-p function) "<compiled function>")
        ((subrp function) (format "<subr %s>" (subr-name function)))
        (t (format "%S" function))))

(defun snapshot--value (value)
  "Return a stable, single-line printed form of VALUE."
  (let ((print-length 40)
        (print-level 6)
        (print-escape-newlines t))
    (thread-last (if (functionp value)
                     (snapshot--function value)
                   (prin1-to-string value))
                 (replace-regexp-in-string "#<[^>]*>" "#<object>")
                 (replace-regexp-in-string "#\\[[^]]*\\]" "#[compiled]")
                 (snapshot--shorten-paths))))

(defun snapshot--binding (binding)
  "Return the printed form of a key BINDING."
  (cond ((null binding) "nil")
        ((symbolp binding) (symbol-name binding))
        ((functionp binding) (snapshot--function binding))
        ((stringp binding) (format "%S" binding))
        (t (snapshot--value binding))))

(defun snapshot--keymap-lines (name keymap)
  "Return one line per binding in KEYMAP, named NAME, parents excluded."
  (let ((lines nil)
        (seen nil))
    (cl-labels
        ((walk (map prefix depth)
           (unless (or (memq map seen) (> depth snapshot-keymap-depth))
             (push map seen)
             (map-keymap-internal
              (lambda (event binding)
                (cond
                 ((memq event '(menu-bar tool-bar tab-bar)))
                 ;; WATCHOUT: Neighbouring characters with the same binding
                 ;; arrive as one range (FROM . TO): C-r and C-s on one
                 ;; command, for instance.  Skipping ranges hid them.
                 ((and (consp event)
                       (<= (- (cdr event) (car event))
                           snapshot-key-range-limit))
                  (dolist (char (number-sequence (car event) (cdr event)))
                    (visit char binding prefix depth)))
                 ((consp event)
                  (push (format "key %s %s..%s %s" name
                                (key-description (vconcat prefix
                                                          (vector (car event))))
                                (key-description (vector (cdr event)))
                                (snapshot--binding binding))
                        lines))
                 (t (visit event binding prefix depth))))
              map)))
         (visit (event binding prefix depth)
           (let ((keys (vconcat prefix (vector event)))
                 (target (if (and (symbolp binding) (fboundp binding))
                             (indirect-function binding)
                           binding)))
             (if (keymapp target)
                 (walk target keys (1+ depth))
               (push (format "key %s %s %s" name (key-description keys)
                             (snapshot--binding binding))
                     lines)))))
      (walk keymap [] 0))
    lines))

(defun snapshot--keymaps ()
  "Return the lines of all keymaps bound to a variable."
  (let (lines)
    (mapatoms
     (lambda (symbol)
       (when (and (boundp symbol)
                  (string-suffix-p "-map" (symbol-name symbol))
                  (keymapp (symbol-value symbol)))
         (setq lines (nconc (snapshot--keymap-lines
                             symbol (symbol-value symbol))
                            lines)))))
    lines))

(defun snapshot--options ()
  "Return one line per user option with its default value."
  (let (lines)
    (mapatoms
     (lambda (symbol)
       (when (and (custom-variable-p symbol) (default-boundp symbol))
         (push (format "option %s %s" symbol
                       (snapshot--value (default-value symbol)))
               lines))))
    lines))

(defun snapshot--hooks ()
  "Return one line per function on a hook, by the hook's default value."
  (let (lines)
    (mapatoms
     (lambda (symbol)
       (when (and (default-boundp symbol)
                  (string-match-p "-\\(hook\\|functions\\)\\'"
                                  (symbol-name symbol)))
         (let ((value (default-value symbol)))
           (dolist (function (cond ((functionp value) (list value))
                                   ((proper-list-p value) value)))
             (when (or (functionp function) (symbolp function))
               (push (format "hook %s %s" symbol
                             (snapshot--function function))
                     lines)))))))
    lines))

(defun snapshot--file-modes ()
  "Return one line per entry of the file-to-mode tables."
  (append
   (mapcar (lambda (entry)
             (format "mode file %S %s"
                     (car entry) (snapshot--value (cdr entry))))
           auto-mode-alist)
   (mapcar (lambda (entry)
             (format "mode interpreter %S %s"
                     (car entry) (snapshot--value (cdr entry))))
           interpreter-mode-alist)))

(defun snapshot-write (file failed)
  "Write the snapshot to FILE; FAILED are packages that did not load."
  (let ((lines (append (mapcar (lambda (name) (format "unloadable %s" name))
                               failed)
                       (snapshot--keymaps)
                       (snapshot--options)
                       (snapshot--hooks)
                       (snapshot--file-modes))))
    ;; NOTE: Key ranges include Emacs' raw-byte characters, which UTF-8
    ;; cannot encode; Emacs would then ask for a coding system.
    (let ((coding-system-for-write 'utf-8-emacs-unix))
      (with-temp-file file
        (insert (string-join (seq-uniq (sort lines #'string<)) "\n") "\n")))
    (length lines)))

(provide 'snapshot)

;;; snapshot.el ends here
