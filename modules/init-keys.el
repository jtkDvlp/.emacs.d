;;; init-keys.el --- Keyboard infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; What binding keys relies on: repeating keys, the global prefixes
;; several modules share, and the macOS modifier layout.  The bindings
;; themselves stay with their topic.

;;; Code:

;; NOTE: After a key of a repeat map its keys work alone, until another
;; key is typed: C-x o o o, or M-+ f + + -.  Emacs brings such maps for
;; windows, undo, errors and more.
(repeat-mode)

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
