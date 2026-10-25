;;; init.el --- Entry point of the configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the configuration modules under modules/, one topic each, in a
;; fixed order.  What has to happen before the first frame is in
;; early-init.el; own libraries the modules use are under lisp/.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; WATCHOUT: The order matters.  init-packages sets up the package
;; manager every later module relies on; and where two modules bind the
;; same global key, the later one wins (plan, B1: `C--').
(require 'init-packages)
(require 'init-core)
(require 'init-keys)
(require 'init-appearance)
(require 'init-minibuffer)
(require 'init-windows)
(require 'init-editing)
(require 'init-diff)
(require 'init-project)
(require 'init-treemacs)
(require 'init-bookmarks)
(require 'init-vc)
(require 'init-org)
(require 'init-startup)
(require 'init-structural-editing)
(require 'init-completion)
(require 'init-checking)
(require 'init-snippets)
(require 'init-restclient)
(require 'init-sql)
(require 'init-httpd)
(require 'init-lang-markdown)
(require 'init-logview)
(require 'init-shell)
(require 'init-lang-csv)
(require 'init-lang-elisp)
(require 'init-lang-typescript)
(require 'init-lang-javascript)
;; (require 'init-lang-php)
(require 'init-lang-python)
(require 'init-lang-html)
(require 'init-lang-css)
(require 'init-lang-clojure)
(require 'init-lang-basic)
(require 'init-lang-c)

(provide 'init)

;;; init.el ends here
