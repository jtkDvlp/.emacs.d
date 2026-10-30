;;; init-lang-javascript.el --- JavaScript -*- lexical-binding: t; -*-

;;; Commentary:

;; js-ts-mode, the tree-sitter mode, with typescript-language-server
;; through eglot and keys for structural editing, and npm's commands.

;;; Code:

(require 'jtk-project)

(defvar eglot-server-programs)

;; WATCHOUT: typescript-language-server brings no TypeScript of its own; it
;; looks for the project's node_modules/typescript and fails to start
;; ("Could not find a valid TypeScript installation") where a project
;; has none, as most plain JavaScript projects.  It is then pointed to a
;; global installation (npm install -g typescript, or brew).
(defun jtk-javascript--global-typescript ()
  "Return the lib directory of a global TypeScript installation, or nil."
  (let ((npm-root (when (executable-find "npm")
                    (string-trim (shell-command-to-string "npm root -g")))))
    (seq-find
     #'file-directory-p
     (delq nil
           (list (when npm-root
                   (expand-file-name "typescript/lib" npm-root))
                 (concat "/opt/homebrew/opt/typescript/libexec/lib/"
                         "node_modules/typescript/lib"))))))

(defun jtk-javascript-server-contact (&optional _interactive)
  "Return how eglot starts typescript-language-server for this project."
  (let ((own-typescript
         (when-let* ((root (jtk-project-root)))
           (file-directory-p (expand-file-name "node_modules/typescript"
                                               root))))
        (global-typescript
         (jtk-javascript--global-typescript)))
    `("typescript-language-server" "--stdio"
      ,@(when (and (not own-typescript) global-typescript)
          `(:initializationOptions (:tsserver (:path ,global-typescript)))))))

(use-package
  js
  :ensure nil

  :init
  (setopt treesit-enabled-modes (cons 'js-ts-mode treesit-enabled-modes))

  ;; NOTE: typescript-language-server, installed outside Emacs, serves
  ;; JavaScript as well; see `jtk-javascript-server-contact'.
  :hook
  (js-ts-mode . eglot-ensure)

  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((js-ts-mode :language-id "javascript")
                   . jtk-javascript-server-contact)))

  (defun jtk-javascript-transpose-line-up ()
    "Move the current line above the previous one."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (indent-according-to-mode))

  (defun jtk-javascript-transpose-line-down ()
    "Move the current line below the next one."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode))

  ;; NOTE: Statements are lines, so moving one up or down transposes
  ;; lines instead of s-expressions.
  (jtk-structural-editing-bind
   js-ts-mode-map
   '(("C-M-<up>" . jtk-javascript-transpose-line-up)
     ("C-M-<down>" . jtk-javascript-transpose-line-down))))

;; NOTE: npm runs in the project's root through `compile', so its output
;; gets a buffer with links to errors; the Node version comes from the
;; project's .envrc (init-core).
(defun jtk-npm--run (arguments)
  "Run npm with ARGUMENTS, a string, in the root of the current project."
  (let ((default-directory (or (jtk-project-root) default-directory)))
    (compile (concat "npm " arguments))))

(defun jtk-npm--scripts ()
  "Return the names of the scripts in the project's package.json."
  (when-let* ((root (jtk-project-root))
              (file (expand-file-name "package.json" root))
              ((file-exists-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (mapcar #'symbol-name
              (map-keys (map-elt (json-parse-buffer :object-type 'alist)
                                 'scripts))))))

(defun jtk-npm-install ()
  "Install the project's dependencies."
  (interactive)
  (jtk-npm--run "install"))

(defun jtk-npm-update ()
  "Update the project's dependencies."
  (interactive)
  (jtk-npm--run "update"))

(defun jtk-npm-run (script)
  "Run SCRIPT from the project's package.json."
  (interactive
   (list (completing-read "npm run: " (jtk-npm--scripts) nil t)))
  (jtk-npm--run (concat "run " (shell-quote-argument script))))

;; NOTE: Under C-c M-j in JavaScript buffers and under j in the project
;; commands, for every project with a package.json.
(defvar-keymap jtk-npm-project-map
  :doc "npm: installing (i), running a script (r), updating (u)."
  :prefix t
  "i" #'jtk-npm-install
  "r" #'jtk-npm-run
  "u" #'jtk-npm-update)

(add-to-list 'jtk-project-kinds
             '("npm" ("package.json") jtk-npm-project-map)
             t)

(with-eval-after-load 'js
  (keymap-set js-ts-mode-map "C-c M-j" 'jtk-npm-project-map))

(provide 'init-lang-javascript)

;;; init-lang-javascript.el ends here
