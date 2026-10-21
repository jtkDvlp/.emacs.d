;; WATCHOUT: elpaca installs and loads packages only after init.  These
;; two are called by top-level code of the modules below (and
;; `:diminish' expands into calls of `diminish'), so they have to be
;; there before that code runs.
(use-package diminish
  :ensure (:wait t)
  :demand t)

(use-package smartrep
  :ensure (:wait t)
  :demand t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize)
  ;; NOTE: Only after reading PATH: `exec-path-from-shell' asks
  ;; `shell-file-name', and PATH is set up for the login shell.
  (setq shell-file-name "/bin/bash"))

(global-so-long-mode 1)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(require 'base--apperance)
(require 'base--interface)
(require 'base--filemanagement)
(require 'base--organisation)
(require 'base--startup)
(require 'base--development)
(require 'base--shell)

(require 'file--csv)

(require 'lang--elisp)
(require 'lang--typescript)
(require 'lang--javascript)
;; (require 'lang--php)
(require 'lang--python)
(require 'lang--html)
(require 'lang--css)
(require 'lang--clojure)
(require 'lang--basic)
(require 'lang--c)

(defun dump (data)
  (setq dump-buffer (get-buffer-create "*dump*"))
  (print data dump-buffer))

(defun replace-keymap-bindings (from to)
  (map-keymap
   (lambda (k f)
     (define-key to (vector k) f))
   from))

(provide 'configuration)
