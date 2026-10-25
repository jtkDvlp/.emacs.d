;;; shim.el --- Cloud sandbox: blocked hosts -*- lexical-binding: t; -*-

;;; Commentary:

;; Only for the cloud environment Claude works in, loaded by
;; dev/cloud/emacs ahead of the check.  The proxy there blocks
;; melpa.org and elpa.gnu.org; elpaca only fetches descriptions and
;; download counts from them (recipes and sources come from GitHub), so
;; these requests are answered empty instead of failing.

;;; Code:

(with-eval-after-load 'elpaca-menu-melpa
  (advice-add 'elpaca-menu-melpa--metadata :override #'ignore))

(defun shim--empty-response (orig url &rest args)
  "Call ORIG with URL and ARGS unless URL is on a blocked ELPA host."
  (if (string-match-p "\\`https://elpa\\.\\(non\\)?gnu\\.org/" url)
      (with-current-buffer (generate-new-buffer " *blocked*")
        (setq-local url-http-end-of-headers (point-min))
        (current-buffer))
    (apply orig url args)))

(advice-add 'url-retrieve-synchronously :around #'shim--empty-response)

;;; shim.el ends here
