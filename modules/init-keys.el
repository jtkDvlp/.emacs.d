;; WATCHOUT: elpaca installs and loads packages only after init, but
;; top-level code of later modules calls `smartrep-define-key', so it
;; has to be there before that code runs.
(use-package smartrep
  :ensure (:wait t)
  :demand t)

(when (eq system-type 'darwin)
  (setq
   ns-alternate-modifier nil
   ns-right-alternate-modifier nil
   mac-command-modifier 'meta))

(provide 'init-keys)
