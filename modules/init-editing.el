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

  :bind*
  (("M-! m s" . start-kbd-macro)
   ("M-! m S" . kmacro-end-macro)
   ("M-! m m" . kmacro-end-and-call-macro)))

(use-package
  ace-jump-mode
  :bind
  (("C-j" . ace-jump-char-mode)
   ("C-S-j" . ace-jump-mode-pop-mark)
   ("M-j" . nil)
   ("M-j l" . ace-jump-line-mode)))

(use-package
  expand-region
  :bind*
  (("C-." . er/expand-region)
   ("C-:" . er/contract-region)))

(use-package
  jtk-narrow
  :ensure nil
  :commands jtk-narrow-dwim
  :bind*
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

  :bind*
  (("M-c m" . mc/mark-more-like-this-extended)
   ("M-c x" . mc/mark-all-like-this-dwim)
   ("M-c a" . mc/mark-all-like-this)
   ("M-c l" . mc/edit-lines)

   ("M-c i n" . mc/insert-numbers)
   ("M-c i l" . mc/insert-letters)
   ("M-c i s" . mc/sort-region)
   ("M-c i r" . mc/reverse-region)

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
