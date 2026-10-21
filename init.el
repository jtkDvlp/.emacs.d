;;; init.el --- Entry point of the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Sets up the package manager, keeps generated files out of the way,
;; then loads the configuration modules.  What has to happen before the
;; first frame is in early-init.el.

;;; Code:

(setq env-jira-host-url nil)

;;;; Package manager

(defvar elpaca-lock-file
  (expand-file-name "etc/elpaca-lock.eld" user-emacs-directory)
  "Commits every package is pinned to.
Written with `elpaca-write-lock-file' after updating packages.")

(defun jtk-init-locked-ref (package)
  "Return the commit `elpaca-lock-file' pins PACKAGE to, or nil."
  (when (file-exists-p elpaca-lock-file)
    (with-temp-buffer
      (insert-file-contents elpaca-lock-file)
      (let ((entry (alist-get package (read (current-buffer)))))
        (plist-get (plist-get entry :recipe) :ref)))))

(defconst jtk-init-elpaca-ref (jtk-init-locked-ref 'elpaca)
  "The commit elpaca itself is pinned to.")

;; NOTE: The installer is upstream's (version 0.12, from elpaca's
;; README) and deliberately left as it is, long lines included, so that
;; it can be compared with a newer one.  The only change is in
;; `elpaca-order': elpaca is pinned to its commit in the lock file like
;; every other package.  A shallow clone (`:depth 1') could not check
;; out an older commit, hence the full clone whenever there is one.
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order `(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref ,jtk-init-elpaca-ref
                              :depth ,(unless jtk-init-elpaca-ref 1)
                              :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; WATCHOUT: Every `use-package' below expands into an elpaca order
;; only once this mode is on; without `:wait' the first declarations
;; would expand before that and fall back to package.el.
(elpaca (elpaca-use-package :wait t)
  (elpaca-use-package-mode))

(setq use-package-always-ensure t
      use-package-verbose t)

;;;; Generated files

;; WATCHOUT: no-littering has to come before every package whose paths
;; it sets; it only changes defaults, so a later `setq' still wins.
(use-package no-littering
  :ensure (:wait t)
  :demand t
  :config
  (setq custom-file (no-littering-expand-var-file-name "custom.el"))
  (load custom-file 'noerror))

;;;; Configuration

(add-to-list 'load-path (expand-file-name "configuration" user-emacs-directory))
(require 'configuration)

(provide 'init)

;;; init.el ends here
