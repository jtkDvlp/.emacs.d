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
  ;; (package-install 'spacemacs-theme)
  (package-install 'doom-themes)
  (package-install 'exec-path-from-shell))

(setq shell-file-name "/bin/bash")

(when (memq window-system '(mac ns x))
  ;; (setq exec-path-from-shell-check-startup-files nil)
  ;; (exec-path-from-shell-initialize)

  (add-to-list 'exec-path "/usr/local/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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
(require 'lang--typescript)
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
