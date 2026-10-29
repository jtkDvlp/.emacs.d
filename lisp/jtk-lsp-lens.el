;;; jtk-lsp-lens.el --- Code lenses of eglot -*- lexical-binding: t; -*-

;;; Commentary:

;; Shows the code lenses of an eglot server at the end of their line, as
;; clojure-lsp's "3 references" after each definition.  eglot itself
;; does not ask for them.
;;
;; Nothing here waits for the server: the lenses are asked for a moment
;; after the last change, and resolved (which gives their text) only for
;; the lines in a window, again when scrolling.
;;
;; NOTE: Measured on a file of 10,000 lines with 2,001 definitions
;; (clojure-lsp 2026.10.15, Emacs 31.1, cloud): clojure-lsp answers
;; with the lenses after 2.4 to 13 s, meanwhile Emacs goes on; their
;; overlays take 10 ms; sending one resolve takes 2 ms, all 2,001 would
;; take 5 s, hence only the visible ones.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'jsonrpc)

(defcustom jtk-lsp-lens-delay 1.0
  "Seconds after the last change before the lenses are asked for again."
  :type 'number
  :group 'eglot)

(defface jtk-lsp-lens-face
  '((t :inherit shadow))
  "Face of a code lens."
  :group 'eglot)

(defvar-local jtk-lsp-lens--lenses nil
  "Lenses of the buffer: a list of (LINE LENS OVERLAY STATE).
LINE starts at 0, as in LSP; STATE is `unresolved', `pending' or
`resolved'.")

(defvar-local jtk-lsp-lens--timer nil)

(defvar-local jtk-lsp-lens--generation 0
  "Counts requests for lenses, so that late answers are dropped.")

(defun jtk-lsp-lens--line (lens)
  "Return the line LENS starts on, counted from 0."
  (plist-get (plist-get (plist-get lens :range) :start) :line))

(defun jtk-lsp-lens--show (entry)
  "Put the title of the resolved lens of ENTRY on its overlay."
  (pcase-let ((`(,_ ,lens ,overlay ,_) entry))
    (when-let* ((title (plist-get (plist-get lens :command) :title)))
      (overlay-put overlay 'after-string
                   (concat "  " (propertize title 'face
                                            'jtk-lsp-lens-face))))))

(defun jtk-lsp-lens--clear ()
  "Remove all lenses of the buffer."
  (dolist (entry jtk-lsp-lens--lenses)
    (delete-overlay (nth 2 entry)))
  (setq jtk-lsp-lens--lenses nil))

(defun jtk-lsp-lens--entries (lenses)
  "Return entries for LENSES, with an overlay at the end of each line.
LENSES is a list of lenses; the buffer is walked once, in line order."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((current 0))
        (mapcar
         (lambda (lens)
           (let ((line (jtk-lsp-lens--line lens)))
             (forward-line (- line current))
             (setq current line)
             (list line lens
                   (make-overlay (line-end-position) (line-end-position)
                                 nil t nil)
                   (if (plist-get lens :command) 'resolved 'unresolved))))
         (sort lenses :key #'jtk-lsp-lens--line))))))

(defun jtk-lsp-lens--visible-p (entry window)
  "Return non-nil if the lens of ENTRY is shown in WINDOW."
  (let ((position (overlay-start (nth 2 entry))))
    (and position
         (>= position (window-start window))
         (<= position (window-end window t)))))

(defun jtk-lsp-lens--resolve (entry)
  "Ask the server for the text of the lens of ENTRY."
  (let ((buffer (current-buffer))
        (generation jtk-lsp-lens--generation))
    (setf (nth 3 entry) 'pending)
    (jsonrpc-async-request
     (eglot-current-server) :codeLens/resolve (nth 1 entry)
     :success-fn
     (lambda (lens)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (= generation jtk-lsp-lens--generation)
             (setf (nth 1 entry) lens
                   (nth 3 entry) 'resolved)
             (jtk-lsp-lens--show entry))))))))

(defun jtk-lsp-lens--resolve-visible (&optional window)
  "Resolve the lenses of the buffer that WINDOW shows."
  (dolist (window (if window (list window)
                    (get-buffer-window-list nil nil t)))
    (dolist (entry jtk-lsp-lens--lenses)
      (when (and (eq (nth 3 entry) 'unresolved)
                 (jtk-lsp-lens--visible-p entry window))
        (jtk-lsp-lens--resolve entry)))))

(defun jtk-lsp-lens--receive (lenses)
  "Replace the buffer's lenses by LENSES, a vector from the server."
  (jtk-lsp-lens--clear)
  (setq jtk-lsp-lens--lenses (jtk-lsp-lens--entries (append lenses nil)))
  (dolist (entry jtk-lsp-lens--lenses)
    (when (eq (nth 3 entry) 'resolved)
      (jtk-lsp-lens--show entry)))
  (jtk-lsp-lens--resolve-visible))

(defun jtk-lsp-lens-refresh ()
  "Ask the server for the lenses of the current buffer."
  (interactive)
  (when-let* ((server (eglot-current-server))
              (file (buffer-file-name)))
    (let ((buffer (current-buffer))
          (generation (cl-incf jtk-lsp-lens--generation)))
      ;; NOTE: `:deferred' holds the request back until eglot has sent
      ;; the latest changes, so the lines of the answer match the buffer.
      (jsonrpc-async-request
       server :textDocument/codeLens
       (list :textDocument (list :uri (eglot-path-to-uri file)))
       :deferred 'jtk-lsp-lens
       :success-fn
       (lambda (lenses)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (= generation jtk-lsp-lens--generation)
               (jtk-lsp-lens--receive lenses)))))))))

(defun jtk-lsp-lens--schedule (&rest _)
  "Refresh the lenses once the buffer has not changed for a while."
  (when (timerp jtk-lsp-lens--timer)
    (cancel-timer jtk-lsp-lens--timer))
  (let ((buffer (current-buffer)))
    (setq jtk-lsp-lens--timer
          (run-with-idle-timer
           jtk-lsp-lens-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (jtk-lsp-lens-refresh))))))))

(defun jtk-lsp-lens--on-scroll (window _start)
  "Resolve the lenses that scrolling brought into WINDOW."
  (with-current-buffer (window-buffer window)
    (jtk-lsp-lens--resolve-visible window)))

(define-minor-mode jtk-lsp-lens-mode
  "Show the code lenses of the language server at the end of their line."
  :lighter nil
  (if jtk-lsp-lens-mode
      (progn
        (add-hook 'after-change-functions #'jtk-lsp-lens--schedule nil t)
        ;; NOTE: A server may answer right after a change with counts from
        ;; before it re-read the file; saving asks once more.
        (add-hook 'after-save-hook #'jtk-lsp-lens--schedule nil t)
        (add-hook 'window-scroll-functions #'jtk-lsp-lens--on-scroll nil t)
        (jtk-lsp-lens--schedule))
    (remove-hook 'after-change-functions #'jtk-lsp-lens--schedule t)
    (remove-hook 'after-save-hook #'jtk-lsp-lens--schedule t)
    (remove-hook 'window-scroll-functions #'jtk-lsp-lens--on-scroll t)
    (when (timerp jtk-lsp-lens--timer)
      (cancel-timer jtk-lsp-lens--timer))
    (cl-incf jtk-lsp-lens--generation)
    (jtk-lsp-lens--clear)))

(defun jtk-lsp-lens-enable-maybe ()
  "Turn on `jtk-lsp-lens-mode' where the server offers code lenses.
Meant for `eglot-managed-mode-hook', which also runs when eglot stops
managing the buffer."
  (jtk-lsp-lens-mode
   (if (and (eglot-managed-p) (eglot-server-capable :codeLensProvider))
       1
     -1)))

(provide 'jtk-lsp-lens)

;;; jtk-lsp-lens.el ends here
