(use-package
  clojure-mode

  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode))

  :config
  (require 'smartparens-clojure)

  :bind*
  (:map clojure-mode-map
	("C-<left>" . sp-backward-sexp)
	("M-<left>" . sp-forward-barf-sexp)
	("C-M-<left>" . sp-backward-barf-sexp)

	("C-<right>" . sp-forward-sexp)
	("M-<right>" . sp-forward-slurp-sexp)
	("C-M-<right>" . sp-backward-slurp-sexp)

	("C-<up>" . sp-backward-up-sexp)
	("M-<up>" . sp-convolute-sexp)

	("C-<down>" . sp-down-sexp)
	("M-<down>" . sp-raise-sexp)

	("C-M-<up>" . sp-backward-transpose-sexp)
	("C-M-<down>" . sp-forward-transpose-sexp)

	("M-\"" . sp-wrap-doublequote)
	("C-(" . sp-wrap-round)
	("M-[" . sp-wrap-square)
	("M-{" . sp-wrap-curly)

	("C-M-(" . sp-rewrap-sexp)

	("C-k" . sp-kill-sexp)
	("C-S-K" . sp-unwrap-sexp)

	("M-k" . sp-splice-sexp-killing-forward)
	("M-K" . sp-splice-sexp-killing-backward)
	("C-M-k" . sp-splice-sexp-killing-around)

	("C-;" . comment-dwim)))

(use-package
  flycheck-clj-kondo
  :after clojure-mode)

(use-package
  clj-refactor
  :after clojure-mode

  :config
  (setq cljr-warn-on-eval nil)
  (cljr-add-keybindings-with-prefix "M-r")
  (clj-refactor-mode 1)

  :bind-keymap*
  ("M-r" . clj-refactor-map))

(use-package
  parseedn)

(use-package
  cider

  :commands
  (cider-jack-in-clj
   cider-connect-clj
   cider-jack-in-cljs
   cider-connect-cljs
   cider-jack-in-clj&cljs
   cider-connect-clj&cljs
   cider-jack-in-with-args
   cider-jack-in-with-profile
   cider-jack-in-dwim)

  :config
  (require 'parseedn)

  (defun get-file-content (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defun lein-project-clj-filepath ()
    (thread-first
        (projectile-project-root)
      (concat "project.clj")))

  (defun lein-project-clj-content (filepath)
    (let* ((data
            (thread-first
                filepath
              (get-file-content)
              (parseedn-read-str)))

           (name
            (nth 1 data))

           (version
            (nth 2 data))

           (rest-root-level-values
            (seq-drop data 3))

           (result
            (make-hash-table :test 'equal)))

      (puthash :name name result)
      (puthash :version version result)

      (mapc (lambda (pair) (puthash (car pair) (car (cdr pair)) result))
            (seq-partition rest-root-level-values 2))

      result))

  (defun lein-project-clj-profiles (filepath)
    (thread-last filepath
      (lein-project-clj-content)
      (gethash :profiles)
      (hash-table-keys)
      (mapcar 'symbol-name)
      (mapcar (lambda (profile) (substring profile 1)))))

  (defun cider-jack-in-with-args (args)
    (interactive "sjack-in repl with args: ")
    (let ((cider-lein-parameters
           args))
      (cider-jack-in nil)))

  (defun cider-jack-in-with-profile (profile)
    (interactive "sjack-in repl with profile: ")
    (cider-jack-in-with-args (concat "with-profile " profile " repl")))

  (defun lein-project-clj-jack-in-profiles ()
    (thread-last
        (lein-project-clj-filepath)
      (lein-project-clj-profiles)
      (seq-filter (lambda (profile) (not (member profile '("dev" "repl" "uberjar")))))
      (seq-map (lambda (profile) (list profile (concat "+" profile))))
      (apply 'append)))

  (defun cider-jack-in-dwim ()
    (interactive)
    (let* ((profiles
            (lein-project-clj-jack-in-profiles))

           (profile
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "jack-in repl with profile: "
                                profiles nil nil nil nil "")))))

      (if profile
          (cider-jack-in-with-profile profile)
        (cider-jack-in nil))))

  (defun cider-eval-dwim ()
    (interactive)
    (save-excursion
      (let ((cursor nil)
            (sexp nil))
        (ignore-errors
          (while (not (string-match-p "^(comment.*" (or sexp "")))
            (setq cursor (point))
            (paredit-backward-up)
            (setq sexp (cider-sexp-at-point))))
        (goto-char cursor)
        (unless (cider-sexp-at-point)
          (paredit-backward-up))
        (cider-eval-sexp-at-point))))

  (defun cider-repl-user-system-start ()
    (interactive)
    (cider-interactive-eval
     "(user/system-restart!)"))

  (defun cider-repl-user-system-stop ()
    (interactive)
    (cider-interactive-eval
     "(user/system-stop!)"))

  (defun cider-repl-user-fig-init ()
    (interactive)
    (cider-interactive-eval
     "(user/fig-init)"))

  (defun cider-repl-refresh-all ()
    (interactive)
    (cider-interactive-eval
     "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))"))

  (defun cider-repl-switch-to-repl-buffer-and-ns ()
    (interactive)
    (call-interactively 'cider-repl-set-ns)
    (call-interactively 'cider-switch-to-repl-buffer))

  (setq
   cider-repl-pop-to-buffer-on-connect 'display-only
   cider-show-error-buffer t
   cider-repl-result-prefix ";; => "
   cider-repl-display-help-banner nil
   cider-prompt-for-symbol nil

   nrepl-log-messages nil
   nrepl-hide-special-buffers t

   cider-repl-use-pretty-printing t
   cider-print-fn 'pprint
   cider-print-options '(("length" 15))

   cider-use-overlays t
   cider-overlays-use-font-lock t

   cider-font-lock-dynamically '(macro core function var)
   cider-repl-use-clojure-font-lock t

   cider-error-highlight-face 'error-face)

  (define-clojure-indent
    (defroutes 'defun)
    (println 'defun)
    (pprint 'defun)
    (lazy-seq'defun)
    (routes 'defun)
    (render 'defun)
    (go 'defun)
    (html 'defun)
    (doall 'defun)
    (dosync 'defun)
    ;; (swap! 'defun)
    ;; (reset! 'defun)

    ;; Custom
    (interval 'defun)
    (routes 'defun)
    (context 'defun)
    (letk 'defun)
    (for-file 'defun)

    ;; Compojure
    (GET 'defun)
    (PUT 'defun)
    (POST 'defun)
    (cp/GET 'defun)
    (cp/PUT 'defun)
    (cp/POST 'defun)
    (where 'defun)
    (add-watch 'defun)
    (listen! 'defun)

    ;; Reframe
    (register-handler 'defun)
    (register-sub 'defun)
    (reg-sub 'defun)
    (reg-event-fx 'defun)
    (reg-event-db 'defun)
    (reg-fx 'defun)
    )

  :bind*
  (:map clojure-mode-map
	("C-c M-j" . nil)

	("C-c M-j J" . cider-jack-in-dwim)
	("C-c M-j j" . cider-connect-clj)

        ("C-c C-M-j" . cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . cider-repl-switch-to-repl-buffer-and-ns)

	("C-c M-j S" . cider-jack-in-cljs)
	("C-c M-j s" . cider-connect-cljs)

	("C-c M-j B" . cider-jack-in-clj&cljs)
	("C-c M-j b" . cider-connect-clj&cljs)

	:map cider-mode-map
	("C-M-g" . cider-interrupt)
        ("C-c C-c" . cider-eval-dwim)
        ("C-M-x" . cider-eval-last-sexp)
	("C-M-S-X" . cider-insert-last-sexp-in-repl)
	("C-c M-k" . cider-load-buffer)

	("C-h h" . cider-doc)
	("C-h H" . cider-javadoc)

	("M-j d" . cider-find-var)
	("M-j D" . cider-grimoire)
	("M-j r" . cider-apropos)
	("M-j f" . cider-find-resource)
	("M-j n" . cider-find-ns)
	("M-J" . cider-pop-back)

	("C-c M-j" . nil)
        ("C-c C-M-j" . cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . cider-repl-switch-to-repl-buffer-and-ns)

	("C-c M-q" . cider-quit)
	("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)
	("C-c M-c" . cider-completion-flush-caches)

        ("M-u" . nil)
        ("M-u s" . cider-repl-user-system-start)
        ("M-u S" . cider-repl-user-system-stop)
        ("M-u f" . cider-repl-user-fig-init)

       	:map cider-repl-mode-map
	("C-<left>" . sp-backward-sexp)
	("M-<left>" . sp-forward-barf-sexp)
	("C-M-<left>" . sp-backward-barf-sexp)

	("C-<right>" . sp-forward-sexp)
	("M-<right>" . sp-forward-slurp-sexp)
	("C-M-<right>" . sp-backward-slurp-sexp)

	("C-<up>" . sp-backward-up-sexp)
	("M-<up>" . sp-convolute-sexp)

	("C-<down>" . sp-down-sexp)
	("M-<down>" . sp-raise-sexp)

	("C-M-<up>" . sp-backward-transpose-sexp)
	("C-M-<down>" . sp-forward-transpose-sexp)

	("M-\"" . sp-wrap-doublequote)
	("C-(" . sp-wrap-round)
	("M-[" . sp-wrap-square)
	("M-{" . sp-wrap-curly)

	("C-M-(" . sp-rewrap-sexp)

	("C-k" . sp-kill-sexp)
	("C-S-K" . sp-unwrap-sexp)

	("M-k" . sp-splice-sexp-killing-forward)
	("M-K" . sp-splice-sexp-killing-backward)
	("C-M-k" . sp-splice-sexp-killing-around)

	("C-;" . sp-comment-dwim)

	("C-<left>" . sp-backward-sexp)
	("M-<left>" . sp-forward-barf-sexp)
	("C-M-<left>" . sp-backward-barf-sexp)

	("C-<right>" . sp-forward-sexp)
	("M-<right>" . sp-forward-slurp-sexp)
	("C-M-<right>" . sp-backward-slurp-sexp)

	("C-<up>" . sp-backward-up-sexp)
	("M-<up>" . sp-convolute-sexp)

	("C-<down>" . sp-down-sexp)
	("M-<down>" . sp-raise-sexp)

	("C-M-<up>" . sp-backward-transpose-sexp)
	("C-M-<down>" . sp-forward-transpose-sexp)

	("M-\"" . sp-wrap-doublequote)
	("C-(" . sp-wrap-round)
	("M-[" . sp-wrap-square)
	("M-{" . sp-wrap-curly)

	("C-M-(" . sp-rewrap-sexp)

	("C-k" . sp-kill-sexp)
	("C-S-K" . sp-unwrap-sexp)

	("M-k" . sp-splice-sexp-killing-forward)
	("M-K" . sp-splice-sexp-killing-backward)
	("C-M-k" . sp-splice-sexp-killing-around)

	("C-;" . sp-comment-dwim)

	("C-c M-j" . nil)
        ("C-c C-M-j" . cider-switch-to-last-clojure-buffer)

	("RET" . cider-repl-return)
	("C-<return>" . cider-repl-newline-and-indent)
	("C-S-<return>" . cider-repl-newline-and-indent)

	;; ("M-p" . history)
	;; ("M-n" . history)

	("M-P" . cider-repl-history)

	("C-M-g" . cider-interrupt)

	("C-h h" . cider-doc)
	("C-h H" . cider-javadoc)

	("M-j d" . cider-find-var)
	("M-j D" . cider-grimoire)
	("M-j r" . cider-apropos)
	("M-j f" . cider-find-resource)
	("M-j n" . cider-find-ns)
	("M-J" . cider-pop-back)

        ("M-u" . nil)
        ("M-u s" . cider-repl-user-system-start)
        ("M-u S" . cider-repl-user-system-stop)
        ("M-u f" . cider-repl-user-fig-init)

	("C-c M-q" . cider-quit)
	("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)

	:map cider-repl-history-mode-map
	("M-p" . cider-repl-history-backward)
	("M-n" . cider-repl-history-forward)

	("C-s" . cider-repl-history-occur)
	("C-r" . nil)

	("C-_" . cider-repl-history-undo-other-window)))

(provide 'lang--clojure)
