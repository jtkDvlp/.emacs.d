;;; init-editing.el --- Editing within a buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Keyboard macros, jumping, growing the region, narrowing, undo,
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

(use-package
  ace-jump-mode
  :bind
  (("C-j" . ace-jump-char-mode)
   ("C-S-j" . ace-jump-mode-pop-mark)

   ;; NOTE: Languages add their jumps to definitions under M-j in their
   ;; own maps; Emacs looks up a prefix in all active maps.
   :prefix-map jtk-jump-map
   :prefix "M-j"
   :prefix-docstring "Jumping: lines here, definitions in the languages."
   ("l" . ace-jump-line-mode)))

(use-package
  expand-region
  :bind
  (("C-." . er/expand-region))

  ;; NOTE: clojure-mode binds C-: as well; the global meaning wins.
  :bind*
  (("C-:" . er/contract-region)))

(use-package
  jtk-narrow
  :ensure nil
  :bind
  (("C--" . jtk-narrow-dwim)
   ("C-+" . jtk-narrow-widen)))

(use-package
  undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode))

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

(use-package
  simple
  :ensure nil
  :hook
  ((before-save . delete-trailing-whitespace)))

(use-package
  aggressive-indent

  :diminish
  aggressive-indent-mode

  :hook
  (prog-mode . aggressive-indent-global-mode)

  :config
  (add-to-list 'aggressive-indent-excluded-modes 'sass-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package
  highlight-symbol
  :demand t
  :diminish highlight-symbol-mode

  :bind
  (("<C-m>" . highlight-symbol))

  :hook
  ((prog-mode . highlight-symbol-mode)
   (cider-repl-mode . highlight-symbol-mode))

  :config
  (setq highlight-symbol-idle-delay 1.5)

  (defvar-keymap jtk-highlight-symbol-map
    :doc "Moving between and replacing the symbol; the keys repeat."
    :prefix t
    :repeat t
    "n" #'highlight-symbol-next
    "p" #'highlight-symbol-prev
    "r" #'highlight-symbol-query-replace)
  (keymap-global-set "M-m" 'jtk-highlight-symbol-map))

(provide 'init-editing)

;;; init-editing.el ends here
