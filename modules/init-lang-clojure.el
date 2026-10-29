;;; init-lang-clojure.el --- Clojure -*- lexical-binding: t; -*-

;;; Commentary:

;; clojure-mode with structural editing keys, clj-kondo, clj-refactor
;; and CIDER: jack-in with Leiningen profiles, figwheel-main builds,
;; starting and stopping the user system, and indentation rules for
;; common macros.

;;; Code:

(require 'jtk-project)

;; NOTE: The same keys under C-c M-j in Clojure buffers and under j in
;; the project commands (C-x x p, and M-x on a project in the project
;; list), there for every project with one of the marker files.
(defvar-keymap jtk-clojure-project-map
  :doc "Starting a REPL (upper case) or connecting to one (lower case)."
  :prefix t
  "J" #'cider-jack-in-dwim
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

  :config
  (jtk-structural-editing-bind clojure-mode-map))

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
  (require 'parseedn)

  (defun nil-blank-string (s)
    (when  s
      (unless (string-blank-p s)
        s)))

  (defun get-file-content (filePath)
    "Return filePath's file content."
    (with-temp-buffer
      (insert-file-contents filePath)
      (buffer-string)))

  (defun lein-project-clj-filepath ()
    (thread-first
      (jtk-project-root)
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
    (let* ((profiles
            (thread-last filepath
                         (lein-project-clj-content)
                         (gethash :profiles))))

      (when profiles
        (thread-last profiles
                     (hash-table-keys)
                     (mapcar 'symbol-name)
                     (mapcar (lambda (profile) (substring profile 1)))))))

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
      (seq-filter (lambda (profile) (not (member profile '("dev" "repl" "provided" "uberjar")))))
      (seq-map (lambda (profile) (list profile (concat "+" profile))))
      (apply 'append)))

  (defun cider-jack-in-dwim ()
    (interactive)
    (let* ((profiles
            (when (file-exists-p (lein-project-clj-filepath))
              (lein-project-clj-jack-in-profiles)))

           (profile
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "jack-in repl with profile: "
                                profiles nil nil nil nil "")))))

      (if profile
          (cider-jack-in-with-profile profile)
        (cider-jack-in nil))))

  (defun cider-jack-in-cljs-dwim ()
    (interactive)
    (let ((cider-lein-parameters
           (concat "with-profile -dev,+dev-desktop repl")))
      (cider-jack-in-cljs nil)))

  (defun cider-eval-dwim ()
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

  (defun cider-repl-user-system-start ()
    (interactive)
    (cider-interactive-eval
     "(if-let [system-go (resolve 'user/system-go!)]
        (if (nil? (resolve 'user/emacs-system-go-executed))
          (do
            (intern 'user 'emacs-system-go-executed)
            (system-go))
          (user/system-restart!))
        (user/system-restart!))"))

  (defun cider-repl-user-system-stop ()
    (interactive)
    (cider-interactive-eval
     "(user/system-stop!)"))

  (defun cider-figwheel-main-profiles ()
    (thread-last
      (jtk-project-root)
      (directory-files)
      (seq-filter (lambda (filename) (s-ends-with? ".cljs.edn" filename)))
      (seq-map (lambda (build-filename) (substring build-filename 0 (- 0 (length ".cljs.edn")))))))

  (defun cider-repl-user-fig-init ()
    (interactive)
    (let* ((profiles
            (cider-figwheel-main-profiles))

           (build
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "jack-in cljs-repl with build: "
                                profiles nil nil nil nil "")))))

      (cider-interactive-eval
       (if build
           (format
            "(require 'figwheel.main.api)
             (figwheel.main.api/start \"%s\")"
            build)
         "(user/fig-init)"))))

  (defun cider-repl-user-quit-cljs-repl ()
    (interactive)
    (cider-interactive-eval
     ":cljs/quit"))

  (defun cider-repl-user-switch-cljs-repl ()
    (interactive)
    (let* ((profiles
            (cider-figwheel-main-profiles))

           (build
            (unless (seq-empty-p profiles)
              (nil-blank-string
               (completing-read "switch cljs-repl to build: "
                                profiles nil nil nil nil ""))))

           (build
            (or build "dev")))

      (cider-interactive-eval
       ":cljs/quit")

      (sleep-for 0 100)

      (cider-interactive-eval
       (format
        "(require 'figwheel.main.api)
         (figwheel.main.api/cljs-repl \"%s\")"
        build build))
      ))

  (defun cider-repl-refresh-all ()
    (interactive)
    (cider-interactive-eval
     "(do (require 'clojure.tools.namespace.repl) (clojure.tools.namespace.repl/refresh-all))"))

  (defun cider-repl-switch-to-repl-buffer-and-ns ()
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
   (register 'defun)
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
        ("C-c C-M-S-J" . cider-repl-switch-to-repl-buffer-and-ns)

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
        ("M-u w" . cider-repl-user-switch-cljs-repl)
        ("M-u W" . cider-repl-user-quit-cljs-repl)

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

        ("M-j d" . cider-find-var)
        ("M-j D" . cider-grimoire)
        ("M-j r" . cider-apropos)
        ("M-j f" . cider-find-resource)
        ("M-j n" . cider-find-ns)

        ("M-u" . nil)
        ("M-u s" . cider-repl-user-system-start)
        ("M-u S" . cider-repl-user-system-stop)
        ("M-u f" . cider-repl-user-fig-init)
        ("M-u w" . cider-repl-user-switch-cljs-repl)
        ("M-u W" . cider-repl-user-quit-cljs-repl)

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
