;;; init-editing.el --- Editing within a buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Keyboard macros, jumping (to definitions, references and lines),
;; documentation at point, growing the region, narrowing, undo,
;; multiple cursors, comments, trailing whitespace, indentation and
;; symbol highlighting.  Editing by s-expressions is in
;; init-structural-editing.

;;; Code:

(delete-selection-mode t)

(use-package
  kmacro
  :ensure nil

  :bind
  (:map jtk-run-map
        :prefix-map jtk-macro-map
        :prefix "m"
        :prefix-docstring "Keyboard macros."
        ("s" . start-kbd-macro)
        ("S" . kmacro-end-macro)
        ("m" . kmacro-end-and-call-macro)))

;; NOTE: The same keys jump in every language: each mode, and later
;; each language server, tells xref and eldoc where to look.  A language
;; binds a key in its own map only where it has something better.
(use-package
  xref
  :ensure nil
  :bind
  (("M-J" . xref-go-back)

   :prefix-map jtk-jump-map
   :prefix "M-j"
   :prefix-docstring "Jumping: to definitions (d), references (r), lines (l)."
   ("d" . xref-find-definitions)
   ("r" . xref-find-references)))

(use-package
  eldoc
  :ensure nil
  :bind
  (:map help-map
        ("h" . eldoc-doc-buffer)))

(use-package
  avy
  :bind
  (("C-j" . avy-goto-char)
   ("C-S-j" . avy-pop-mark)

   :map jtk-jump-map
   ("l" . avy-goto-line)))

;; NOTE: Grows the region by subword, word, list, string, comment,
;; paragraph or defun, and by tree-sitter nodes where a mode has them.
(use-package
  expreg
  :bind
  (("C-." . expreg-expand))

  ;; NOTE: clojure-mode binds C-: as well; the global meaning wins.
  :bind*
  (("C-:" . expreg-contract)))

(use-package
  jtk-narrow
  :ensure nil
  :bind
  (("C--" . jtk-narrow-dwim)
   ("C-+" . jtk-narrow-widen)))

;; NOTE: Emacs' own undo: C-/ undoes, C-? redoes (`undo-redo'), and
;; vundo shows the tree of changes to walk through.  M-_ redoes as well.
(keymap-global-set "M-_" #'undo-redo)

(use-package
  vundo
  :bind
  (("C-x u" . vundo)))

(use-package
  multiple-cursors

  :init
  ;; NOTE: no-littering would put this file under var/; it records which
  ;; commands run for all cursors, a choice made by hand, so it is kept
  ;; under etc/ and versioned.
  (setq mc/list-file (no-littering-expand-etc-file-name "mc-list.el"))

  :config
  (define-key mc/keymap (kbd "<return>") nil)

  :bind
  (:prefix-map jtk-multiple-cursors-map
   :prefix "M-c"
   :prefix-docstring "Multiple cursors: marking (m x a l), inserting (i)."
   ("m" . mc/mark-more-like-this-extended)
   ("x" . mc/mark-all-like-this-dwim)
   ("a" . mc/mark-all-like-this)
   ("l" . mc/edit-lines)

   ("i n" . mc/insert-numbers)
   ("i l" . mc/insert-letters)
   ("i s" . mc/sort-region)
   ("i r" . mc/reverse-region)

   :map mc/keymap
   ("<tab>" . mc-hide-unmatched-lines-mode)))

(use-package
  newcomment
  :ensure nil
  :bind
  (("C-;" . comment-dwim)))

;; NOTE: Only in code: in Markdown, two spaces at the end of a line are
;; a line break.
(defun jtk-editing-delete-trailing-whitespace-on-save ()
  "Delete trailing whitespace whenever the current buffer is saved."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'jtk-editing-delete-trailing-whitespace-on-save)

(use-package
  aggressive-indent

  :diminish
  aggressive-indent-mode

  :demand t

  :config
  ;; NOTE: Indenting after an undo is a new change, so the next undo
  ;; would only take back the indentation.
  (add-to-list 'aggressive-indent-protected-commands 'undo-redo)
  (add-to-list 'aggressive-indent-protected-commands 'undo-only)
  (add-to-list 'aggressive-indent-excluded-modes 'sass-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

  ;; NOTE: In every buffer from the start, apart from the excluded modes,
  ;; text modes and read-only buffers.
  (global-aggressive-indent-mode))

(use-package
  symbol-overlay
  :demand t
  :diminish symbol-overlay-mode

  :init
  ;; NOTE: Otherwise single letters on a marked symbol run commands (n
  ;; jumps, r renames) instead of inserting; the keys are under M-m.  A
  ;; plain variable, hence `setq' before loading.
  (setq symbol-overlay-inhibit-map t)

  :custom
  (symbol-overlay-idle-time 1.5)

  :bind
  (("<C-m>" . symbol-overlay-put))

  :hook
  ((prog-mode . symbol-overlay-mode)
   (cider-repl-mode . symbol-overlay-mode))

  :config
  (defvar-keymap jtk-highlight-symbol-map
    :doc "Moving between and replacing the symbol; the keys repeat."
    :prefix t
    :repeat t
    "n" #'symbol-overlay-jump-next
    "p" #'symbol-overlay-jump-prev
    "r" #'symbol-overlay-query-replace)
  (keymap-global-set "M-m" 'jtk-highlight-symbol-map))

(provide 'init-editing)

;;; init-editing.el ends here
