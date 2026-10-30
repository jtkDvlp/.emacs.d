;;; init-lang-clojure.el --- Clojure -*- lexical-binding: t; -*-

;;; Commentary:

;; clojure-mode with structural editing keys, clojure-lsp and CIDER:
;; jack-in with Leiningen profiles, figwheel-main builds, starting and
;; stopping the user system, and indentation rules for common macros.

;;; Code:

(require 'jtk-project)

;; NOTE: The same keys under C-c M-j in Clojure buffers and under j in
;; the project commands (C-x x p, and M-x on a project in the project
;; list), there for every project with one of the marker files.
(defvar-keymap jtk-clojure-project-map
  :doc "Starting a REPL (upper case) or connecting to one (lower case)."
  :prefix t
  "J" #'jtk-clojure-jack-in-dwim
  "j" #'cider-connect-clj
  "S" #'cider-jack-in-cljs
  "s" #'cider-connect-cljs
  "B" #'cider-jack-in-clj&cljs
  "b" #'cider-connect-clj&cljs)

(add-to-list 'jtk-project-kinds
             '("Clojure" ("project.clj" "deps.edn" "shadow-cljs.edn" "bb.edn")
               jtk-clojure-project-map)
             t)

(use-package
  clojure-mode

  :mode
  (("\\.clj\\'" . clojure-mode)
   ("\\.cljs\\'" . clojurescript-mode))

  ;; NOTE: clojure-lsp, next to CIDER: it works without a REPL and
  ;; brings clj-kondo's findings.  Installed outside Emacs (brew).
  :hook
  (clojure-mode . eglot-ensure)

  :config
  (jtk-structural-editing-bind clojure-mode-map))

(use-package
  cider

  :commands
  (cider-jack-in-clj
   cider-connect-clj
   cider-jack-in-cljs
   cider-connect-cljs
   cider-jack-in-clj&cljs
   cider-connect-clj&cljs
   jtk-clojure-jack-in-with-args
   jtk-clojure-jack-in-with-profile
   jtk-clojure-jack-in-dwim)

  :init
  ;; WATCHOUT: CIDER puts `cider-jack-in-clj' on C-c M-j in
  ;; `clojure-mode-map' twice: from its autoloads when clojure-mode
  ;; loads, and again when cider.el itself loads.  Bound once in :bind,
  ;; the key lost against the second, and C-c M-j only ever jacked in.
  ;; So it is set again after each of the two.
  (defun jtk-clojure-bind-project-keys ()
    (keymap-set clojure-mode-map "C-c M-j" 'jtk-clojure-project-map))
  (with-eval-after-load 'clojure-mode
    (jtk-clojure-bind-project-keys))
  (with-eval-after-load 'cider
    (jtk-clojure-bind-project-keys))

  :config
  (defun jtk-clojure--nil-if-blank (string)
    "Return STRING, or nil if it is empty or only whitespace."
    (unless (or (null string) (string-blank-p string))
      string))

  (defun jtk-clojure--project-clj ()
    "Return the project.clj of the current project, or nil."
    (when-let* ((root (jtk-project-root))
                (file (expand-file-name "project.clj" root))
                ((file-exists-p file)))
      file))

  ;; WATCHOUT: project.clj is code, not EDN: #(...), ~(...) and the like
  ;; made an EDN reader fail on files that Leiningen reads fine.  So the
  ;; profiles are found by their parens, with Clojure's syntax table.
  (defun jtk-clojure-lein-profiles (file)
    "Return the names of the profiles in the Leiningen project FILE.
Signals an error if the :profiles map cannot be read."
    (with-temp-buffer
      (insert-file-contents file)
      (with-syntax-table clojure-mode-syntax-table
        (goto-char (point-min))
        (let (found)
          (while (and (not found)
                      (re-search-forward "\\_<:profiles\\_>" nil t))
            (setq found (not (nth 8 (syntax-ppss)))))
          (when found
            (forward-comment (buffer-size))
            (unless (eq (char-after) ?{)
              (user-error ":profiles in %s is not a map" file))
            (forward-char)
            (let (names)
              (while (progn (forward-comment (buffer-size))
                            (not (memq (char-after) '(?} nil))))
                (let ((start (point)))
                  (forward-sexp)
                  (push (string-remove-prefix
                         ":" (buffer-substring-no-properties start (point)))
                        names))
                (forward-comment (buffer-size))
                ;; NOTE: Metadata, as ^:replace, comes before the value.
                (while (eq (char-after) ?^)
                  (forward-char)
                  (forward-sexp)
                  (forward-comment (buffer-size)))
                (forward-sexp))
              (nreverse names)))))))

  (defun jtk-clojure--jack-in-profiles ()
    "Return the profiles of the project to offer for jack-in, as +NAME too.
Leiningen's own profiles are left out.  If project.clj cannot be read,
say so and return nil."
    (when-let* ((file (jtk-clojure--project-clj)))
      (condition-case err
          (thread-last
            (jtk-clojure-lein-profiles file)
            (seq-remove (lambda (profile)
                          (member profile
                                  '("dev" "repl" "provided" "uberjar"))))
            (seq-mapcat (lambda (profile)
                          (list profile (concat "+" profile)))))
        (error
         (message "Profiles of %s not read: %s"
                  file (error-message-string err))
         nil))))

  (defun jtk-clojure-jack-in-with-args (args)
    "Start a REPL with the Leiningen arguments ARGS."
    (interactive "sjack-in repl with args: ")
    (let ((cider-lein-parameters args))
      (cider-jack-in nil)))

  (defun jtk-clojure-jack-in-with-profile (profile)
    "Start a REPL with the Leiningen PROFILE."
    (interactive "sjack-in repl with profile: ")
    (jtk-clojure-jack-in-with-args (concat "with-profile " profile " repl")))

  (defun jtk-clojure-jack-in-dwim ()
    "Start a REPL, offering the project's Leiningen profiles if any."
    (interactive)
    (let* ((profiles
            (jtk-clojure--jack-in-profiles))

           (profile
            (when profiles
              (jtk-clojure--nil-if-blank
               (completing-read "jack-in repl with profile: "
                                profiles nil nil nil nil "")))))

      (if profile
          (jtk-clojure-jack-in-with-profile profile)
        (cider-jack-in nil))))

  (defun jtk-clojure-jack-in-cljs-dwim ()
    "Start a ClojureScript REPL with the desktop development profile."
    (interactive)
    (let ((cider-lein-parameters "with-profile -dev,+dev-desktop repl"))
      (cider-jack-in-cljs nil)))

  (defun jtk-clojure-eval-dwim ()
    "Evaluate the form inside a `comment' at point, else the top form.
A test definition is run as a test as well."
    (interactive)
    (save-excursion
      (let ((cursor nil)
            (sexp nil))
        (ignore-errors
          (while (not (string-match-p "^(comment.*" (or sexp "")))
            (setq cursor (point))
            (backward-up-list 1 t t)
            (setq sexp (cider-sexp-at-point))))
        (goto-char cursor)
        (unless (cider-sexp-at-point)
          (backward-up-list 1 t t))
        (cider-eval-sexp-at-point)
        (when (string-match-p "^(\\([^\/]*\/\\)?deftest" (cider-sexp-at-point))
          (cider-test-run-test)))))

  (defun jtk-clojure-user-system-start ()
    "Start the user system, or restart it if it runs."
    (interactive)
    (cider-interactive-eval
     "(if-let [system-go (resolve 'user/system-go!)]
        (if (nil? (resolve 'user/emacs-system-go-executed))
          (do
            (intern 'user 'emacs-system-go-executed)
            (system-go))
          (user/system-restart!))
        (user/system-restart!))"))

  (defun jtk-clojure-user-system-stop ()
    "Stop the user system."
    (interactive)
    (cider-interactive-eval
     "(user/system-stop!)"))

  (defun jtk-clojure--figwheel-builds ()
    "Return the figwheel-main builds of the project: its *.cljs.edn files."
    (thread-last
      (jtk-project-root)
      (directory-files)
      (seq-filter (lambda (filename)
                    (string-suffix-p ".cljs.edn" filename)))
      (seq-map (lambda (filename)
                 (string-remove-suffix ".cljs.edn" filename)))))

  (defun jtk-clojure-figwheel-start ()
    "Start a figwheel-main build, chosen among the project's."
    (interactive)
    (let* ((builds
            (jtk-clojure--figwheel-builds))

           (build
            (when builds
              (jtk-clojure--nil-if-blank
               (completing-read "jack-in cljs-repl with build: "
                                builds nil nil nil nil "")))))

      (cider-interactive-eval
       (if build
           (format
            "(require 'figwheel.main.api)
             (figwheel.main.api/start \"%s\")"
            build)
         "(user/fig-init)"))))

  (defun jtk-clojure-figwheel-quit ()
    "Leave the ClojureScript REPL."
    (interactive)
    (cider-interactive-eval
     ":cljs/quit"))

  (defun jtk-clojure-figwheel-switch ()
    "Switch the ClojureScript REPL to another figwheel-main build."
    (interactive)
    (let* ((builds
            (jtk-clojure--figwheel-builds))

           (build
            (or (when builds
                  (jtk-clojure--nil-if-blank
                   (completing-read "switch cljs-repl to build: "
                                    builds nil nil nil nil "")))
                "dev")))

      (cider-interactive-eval
       ":cljs/quit")

      (sleep-for 0 100)

      (cider-interactive-eval
       (format
        "(require 'figwheel.main.api)
         (figwheel.main.api/cljs-repl \"%s\")"
        build))))

  (defun jtk-clojure-refresh-all ()
    "Reload all namespaces with tools.namespace."
    (interactive)
    (cider-interactive-eval
     "(do (require 'clojure.tools.namespace.repl)
          (clojure.tools.namespace.repl/refresh-all))"))

  (defun jtk-clojure-switch-to-repl-and-ns ()
    "Switch to the REPL, in the namespace of the current buffer."
    (interactive)
    (call-interactively 'cider-repl-set-ns)
    (call-interactively 'cider-switch-to-repl-buffer))

  (setq
   cider-default-cljs-repl 'figwheel-main

   cider-repl-pop-to-buffer-on-connect nil
   cider-show-error-buffer nil
   cider-repl-result-prefix ";; => "
   cider-repl-display-help-banner nil
   cider-prompt-for-symbol nil

   nrepl-log-messages t
   nrepl-hide-special-buffers nil
   nrepl-use-ssh-fallback-for-remote-hosts t

   ;; cider-repl-use-pretty-printing t
   ;; cider-print-fn 'pprint
   cider-print-options '(("length" 15))

   ;; NOTE: CIDER's answers for M-j d and M-j r come after clojure-lsp's,
   ;; which knows the whole project and needs no REPL; in the REPL buffer,
   ;; where clojure-lsp does not run, CIDER answers.
   cider-xref-fn-depth 90

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
   (render 'defun)
   (go 'defun)
   (html 'defun)
   (doall 'defun)
   (dosync 'defun)
   (log/spy 'defun)
   (log/debug 'defun)
   (log/trace 'defun)
   ;; (swap! 'defun)
   ;; (reset! 'defun)
   (is 'defun)
   (testing 'defun)
   (element 'defun)
   (match 'defun)
   (->files 'defun)
   (->dir 'defun)
   (assoc 'defun)

   ;; Custom
   (interval 'defun)
   (routes 'defun)
   (context 'defun)
   (letk 'defun)
   (for-file 'defun)
   (entity 'defun)
   (type 'defun)
   (usage 'defun)
   (register 'defun)
   (<with-transaction 'defun)
   (<!with-transaction 'defun)
   (<with-temporary-db-file 'defun)
   (<with-attached-db 'defun)
   (<with-keep-awake 'defun)
   (<with-resource 'defun)
   (<!with-resource 'defun)
   (<with-connection 'defun)
   (<!with-connection 'defun)
   (some-interact 'defun)
   (commit! 'defun)

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
   (reg-sub-raw 'defun)
   (reg-sub 'defun)
   (reg-event-fx 'defun)
   (reg-event-db 'defun)
   (reg-fx 'defun)
   (reg-cofx 'defun)
   (reg-acofx 'defun)
   (reg-acofx-by-fx 'defun)
   (as-task 'defun)
   (reg 'defun)
   (reg-remote-sub 'defun)
   (reg-remote-sub-raw 'defun)
   )

  ;; NOTE: In the REPL comments go through smartparens, which keeps the
  ;; parens of the input balanced.
  (jtk-structural-editing-bind
   cider-repl-mode-map
   '(("C-;" . sp-comment-dwim)))

  :bind
  (:map clojure-mode-map
        ("C-c C-M-j" . cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . jtk-clojure-switch-to-repl-and-ns)

        :map cider-mode-map
        ("C-M-g" . cider-interrupt)
        ("C-c C-c" . jtk-clojure-eval-dwim)
        ("C-M-x" . cider-eval-last-sexp)
        ("C-M-S-X" . cider-insert-last-sexp-in-repl)
        ("C-c M-k" . cider-load-buffer)

        ("C-h h" . cider-doc)
        ("C-h H" . cider-javadoc)

        ("M-j a" . cider-apropos)
        ("M-j f" . cider-find-resource)
        ("M-j n" . cider-find-ns)

        ("C-c M-j" . nil)
        ("C-c C-M-j" . cider-switch-to-repl-buffer)
        ("C-c C-M-S-J" . jtk-clojure-switch-to-repl-and-ns)

        ("C-c M-q" . cider-quit)
        ("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)
        ("C-c M-c" . cider-completion-flush-caches)

        ("M-u" . nil)
        ("M-u s" . jtk-clojure-user-system-start)
        ("M-u S" . jtk-clojure-user-system-stop)
        ("M-u f" . jtk-clojure-figwheel-start)
        ("M-u w" . jtk-clojure-figwheel-switch)
        ("M-u W" . jtk-clojure-figwheel-quit)

        :map cider-repl-mode-map
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

        ("M-j a" . cider-apropos)
        ("M-j f" . cider-find-resource)
        ("M-j n" . cider-find-ns)

        ("M-u" . nil)
        ("M-u s" . jtk-clojure-user-system-start)
        ("M-u S" . jtk-clojure-user-system-stop)
        ("M-u f" . jtk-clojure-figwheel-start)
        ("M-u w" . jtk-clojure-figwheel-switch)
        ("M-u W" . jtk-clojure-figwheel-quit)

        ("C-c M-q" . cider-quit)
        ("C-c M-r" . cider-restart)
        ("C-c M-o" . cider-find-and-clear-repl-output)

        :map cider-repl-history-mode-map
        ("M-p" . cider-repl-history-backward)
        ("M-n" . cider-repl-history-forward)

        ("C-s" . cider-repl-history-occur)
        ("C-r" . nil)

        ("C-_" . cider-repl-history-undo-other-window)))

(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here
