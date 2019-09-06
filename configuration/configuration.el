(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))

      package-enable-at-startup nil)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'diminish)
  (package-install 'smartrep)
  (package-install 'use-package)
  (package-install 'spacemacs-theme))

(require 'smartrep);
(require 'use-package)

(setq
 use-package-verbose t
 use-package-always-ensure t

 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(require 'base--apperance)
(require 'base--interface)
(require 'base--filemanagement)
(require 'base--organisation)
(require 'base--startup)
(require 'base--development)

(require 'lang--elisp)
;; (require 'lang--typescript)
(require 'lang--javascript)
;; (require 'lang--php)
(require 'lang--python)
(require 'lang--html)
(require 'lang--css)
(require 'lang--clojure)

(defun dump (data)
  (setq dump-buffer (get-buffer-create "*dump*"))
  (print data dump-buffer))

(provide 'configuration)
