;;; init-keys.el --- Keyboard infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; What binding keys relies on: repeating keys, the help after a prefix
;; (with embark, init-minibuffer), the global prefixes several modules
;; share, and the macOS modifier layout.  The bindings themselves stay
;; with their topic.
;;
;; Each module names the prefix maps it owns, so that the help after a
;; prefix and `describe-keymap' show what a prefix is for.  A plain
;; prefix map is declared with use-package's `:prefix-map', which also
;; autoloads its commands; one whose keys repeat is a `defvar-keymap'
;; with `:repeat', since `:prefix-map' cannot make keys repeat.

;;; Code:

;; NOTE: After a key of a repeat map its keys work alone, until another
;; key is typed: C-x o o o, or M-+ f + + -.  Emacs brings such maps for
;; windows, undo, errors and more.
(repeat-mode)

(defvar-keymap jtk-run-map
  :doc "Running things: keyboard macros (m) and shells (s)."
  :prefix t)
(keymap-global-set "M-!" 'jtk-run-map)

(defvar-keymap jtk-view-map
  :doc "The frame: font size (f) and size of the frame (w)."
  :prefix t)
(keymap-global-set "M-+" 'jtk-view-map)

;; NOTE: In the GUI C-m is a key of its own instead of RET, so it can be
;; bound (init-editing).  A terminal sends the same byte for both.
(define-key input-decode-map [?\C-m] [C-m])

(setq
 ns-alternate-modifier nil
 ns-right-alternate-modifier nil
 mac-command-modifier 'meta)

(provide 'init-keys)

;;; init-keys.el ends here
