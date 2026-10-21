;;; early-init.el --- Before the first frame -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs before packages are activated and before the first frame is
;; drawn.  Only what has to happen that early belongs here: switching
;; off package.el in favour of elpaca, and frame settings that would
;; otherwise flicker into view and out again.

;;; Code:

;; elpaca manages the packages (see init.el); package.el must not
;; activate anything first.
(setq package-enable-at-startup nil)

;; NOTE: Frame parameters rather than calling `menu-bar-mode' and the
;; like: the first frame is created from them without ever drawing the
;; bars, and a build without a GUI has no `scroll-bar-mode' to call.
;; The variables are set as well, so that toggling the modes later
;; starts from the right state.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(push '(fullscreen . maximized) initial-frame-alist)

;;; early-init.el ends here
