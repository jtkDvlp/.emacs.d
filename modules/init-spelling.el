;;; init-spelling.el --- Spelling -*- lexical-binding: t; -*-

;;; Commentary:

;; Wherever text is written, in text modes and in the comments and
;; strings of code: jinx underlines misspelled words, German and English
;; at once, and M-$ offers corrections; the completion pop-up offers
;; words of both dictionaries.  The word lists for that are made from
;; aspell's dictionaries the first time they are needed, into var/dict/.

;;; Code:

(declare-function no-littering-expand-var-file-name "no-littering")
(declare-function cape-dict "cape")
(declare-function cape-dabbrev "cape")
(declare-function cape-wrap-super "cape")
(declare-function jinx--load-module "jinx")

(defconst jtk-spelling-languages '("de" "en")
  "aspell dictionaries whose words the completion offers.")

;; NOTE: Made in 0.3 s for German (365 000 words, 4.6 MB) and 0.1 s for
;; English (127 000 words, 1.2 MB); a lookup in them takes 4 ms (aspell
;; 0.60, cloud).  Hence made when first needed, not shipped.
(defun jtk-spelling--make-word-list (language file)
  "Write the words of aspell's LANGUAGE dictionary to FILE.
Return non-nil on success; nil without aspell or that dictionary."
  (when (executable-find "aspell")
    (make-directory (file-name-directory file) t)
    (let ((part (concat file ".part")))
      (call-process-shell-command
       (format "aspell -d %s dump master | aspell -l %s expand \
| tr ' ' '\\n' | sort -u > %s"
               language language (shell-quote-argument part)))
      (if (> (or (file-attribute-size (file-attributes part)) 0) 0)
          (progn (rename-file part file t) t)
        (ignore-errors (delete-file part))
        nil))))

(defun jtk-spelling-word-lists ()
  "Return the word lists of `jtk-spelling-languages', made if missing.
For `cape-dict-file'."
  (let (files)
    (dolist (language jtk-spelling-languages (nreverse files))
      (let ((file (no-littering-expand-var-file-name
                   (format "dict/%s.txt" language))))
        (when (or (file-exists-p file)
                  (jtk-spelling--make-word-list language file))
          (push file files))))))

(defun jtk-spelling-prose-p ()
  "Return non-nil if point is in prose: a text mode, or a comment or
string of code."
  (or (derived-mode-p 'text-mode)
      (nth 8 (syntax-ppss))))

(defun jtk-spelling-complete-word ()
  "Complete a word in prose from the open buffers and the dictionaries.
Elsewhere nil, so the next function of `completion-at-point-functions'
answers."
  (when (jtk-spelling-prose-p)
    (cape-wrap-super #'cape-dabbrev #'cape-dict)))

(with-eval-after-load 'cape
  (setopt cape-dict-file #'jtk-spelling-word-lists))

;; NOTE: Before the plain words of the open buffers (init-completion,
;; depth 90), after file names (80); in code only within comments and
;; strings.
(add-hook 'completion-at-point-functions #'jtk-spelling-complete-word 85)

;; WATCHOUT: jinx compiles a module against enchant when first used.
;; Without enchant that fails, and switched on regardless, the error
;; broke off the start and the switching on of other modes, highlighting
;; among them.  So jinx comes on only once its module loads.
(defun jtk-spelling-enable-checking ()
  "Turn on `global-jinx-mode' if jinx's module loads, else say why not."
  (condition-case err
      (progn
        (jinx--load-module)
        (global-jinx-mode 1))
    (error
     (message "Spelling not checked (%s); brew install enchant pkgconf"
              (error-message-string err)))))

;; NOTE: In code jinx checks comments and strings only (its
;; `jinx-include-faces'), never the names of the code itself.
(use-package
  jinx
  :custom
  (jinx-languages "de_DE en_US")
  :hook
  (elpaca-after-init . jtk-spelling-enable-checking)
  :bind
  (("M-$" . jinx-correct)
   ("C-M-$" . jinx-languages)))

(provide 'init-spelling)

;;; init-spelling.el ends here
