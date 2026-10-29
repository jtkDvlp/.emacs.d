;;; init-completion.el --- Completion in buffers -*- lexical-binding: t; -*-

;;; Commentary:

;; corfu: a pop-up with the candidates of `completion-at-point', matched
;; with the same completion styles as the minibuffer (orderless), with
;; documentation next to it.  cape adds words of the open buffers and
;; file names where a mode has nothing to offer.

;;; Code:

(use-package
  corfu
  :demand t

  :custom
  ;; NOTE: The pop-up opens by itself after 0.2 s once three characters
  ;; are typed.
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 3)
  (corfu-popupinfo-delay 0.2)
  ;; NOTE: Emacs 30 offers words of the spelling dictionary in text
  ;; modes, which fails without one and crowds out the words from cape.
  (text-mode-ispell-word-completion nil)

  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  ;; NOTE: Candidates picked before come first; savehist keeps them
  ;; across sessions.
  (corfu-history-mode)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; NOTE: These come last in `completion-at-point-functions', so they
;; only answer where the mode's own completion has nothing at point: in
;; text, comments and shells, file names first, then words of the open
;; text and code buffers, the current one first.
(use-package
  cape
  :demand t

  :custom
  (cape-dabbrev-buffer-function #'cape-text-buffers)

  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  (add-hook 'completion-at-point-functions #'cape-file 80))

(provide 'init-completion)

;;; init-completion.el ends here
