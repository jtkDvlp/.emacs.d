((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id ace-window
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (aggressive-indent :source "elpaca-menu-lock-file" :recipe
                    (:package "aggressive-indent" :repo
                              "Malabarba/aggressive-indent-mode"
                              :fetcher github :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id
                              aggressive-indent :type git :protocol
                              https :inherit t :depth treeless :ref
                              "a437a45868f94b77362c6b913c5ee8e67b273c42"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id async :type git
                  :protocol https :inherit t :depth treeless :ref
                  "4fdcb061a166e0d6ccc27d3829a28e04415ae825"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "elpaca-menu-lock-file" :id avy :type git
                :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (buffer-move :source "elpaca-menu-lock-file" :recipe
              (:package "buffer-move" :fetcher github :repo
                        "lukhas/buffer-move" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id
                        buffer-move :type git :protocol https :inherit
                        t :depth treeless :ref
                        "e7800b3ab1bd76ee475ef35507ec51ecd5a3f065"))
 (cfrs :source "elpaca-menu-lock-file" :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "elpaca-menu-lock-file" :id cfrs :type git
                 :protocol https :inherit t :depth treeless :ref
                 "981bddb3fb9fd9c58aed182e352975bd10ad74c8"))
 (cider :source "elpaca-menu-lock-file" :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider"
                  :files
                  ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh"
                   "lein.sh" (:exclude ".dir-locals.el"))
                  :old-names (nrepl) :source "elpaca-menu-lock-file"
                  :id cider :type git :protocol https :inherit t
                  :depth treeless :ref
                  "d35d5567ac1daaa50398d8d5a40f0620ee367038"))
 (clj-refactor :source "elpaca-menu-lock-file" :recipe
               (:package "clj-refactor" :fetcher github :repo
                         "clojure-emacs/clj-refactor.el" :files
                         (:defaults "CHANGELOG.md") :source
                         "elpaca-menu-lock-file" :id clj-refactor
                         :type git :protocol https :inherit t :depth
                         treeless :ref
                         "2805bd5f505fdb199a8c5a25fca398ec9c161e5b"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo
                         "clojure-emacs/clojure-mode" :fetcher github
                         :files ("clojure-mode.el") :source
                         "elpaca-menu-lock-file" :id clojure-mode
                         :type git :protocol https :inherit t :depth
                         treeless :ref
                         "3c68569738f04a22d52f1ca28f593c2ee733bf04"))
 (company :source "elpaca-menu-lock-file" :recipe
          (:package "company" :fetcher github :repo
                    "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small"
                                "doc/images/small/*.png"))
                    :source "elpaca-menu-lock-file" :id company :type
                    git :protocol https :inherit t :depth treeless
                    :ref "e50ea7a3222020cef8e7f320f9870bb20c61e451"))
 (company-quickhelp :source "elpaca-menu-lock-file" :recipe
                    (:package "company-quickhelp" :fetcher github
                              :repo "company-mode/company-quickhelp"
                              :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "elpaca-menu-lock-file" :id
                              company-quickhelp :type git :protocol
                              https :inherit t :depth treeless :ref
                              "5bda859577582cc42d16fc0eaf5f7c8bedfd9e69"))
 (compat :source "elpaca-menu-lock-file" :recipe
         (:package "compat" :repo
                   ("https://github.com/emacs-compat/compat"
                    . "compat")
                   :tar nil :host gnu :files ("*" (:exclude ".git"))
                   :source "elpaca-menu-lock-file" :id compat :type
                   git :protocol https :inherit t :depth treeless :ref
                   "90880f81419577e1d3f68424d2a3adf31e6d663e"))
 (cond-let :source "elpaca-menu-lock-file" :recipe
           (:package "cond-let" :fetcher github :repo
                     "tarsius/cond-let" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id cond-let
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "09292a77001434f59ab55c775dec2b98cb18d028"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id consult :type
                    git :protocol https :inherit t :depth treeless
                    :ref "a64569f377b8bc0f62e1d2ae0f3ee48ba4a7cc1c"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa"
                      . "csv-mode")
                     :tar nil :host gnu :branch "externals/csv-mode"
                     :files ("*" (:exclude ".git")) :source
                     "elpaca-menu-lock-file" :id csv-mode :type git
                     :protocol https :inherit t :depth treeless :ref
                     "a693259ddef057eda82421b2a4410b3e35832e91"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source
                 "elpaca-menu-lock-file" :id dash :type git :protocol
                 https :inherit t :depth treeless :ref
                 "d746dd9edcb67a108818beb0cdc78dc1cb466832"))
 (dashboard :source "elpaca-menu-lock-file" :recipe
            (:package "dashboard" :fetcher github :repo
                      "emacs-dashboard/dashboard" :files
                      (:defaults "banners") :source
                      "elpaca-menu-lock-file" :id dashboard :type git
                      :protocol https :inherit t :depth treeless :ref
                      "a2c49ba27f3a906fd8b059da55104c1ec562b8b6"))
 (datetime :source "elpaca-menu-lock-file" :recipe
           (:package "datetime" :fetcher github :repo
                     "doublep/datetime" :files (:defaults "*.extmap")
                     :source "elpaca-menu-lock-file" :id datetime
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "2601120d4d2857cdbad1cf8d4b84d77308920835"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo
                     "myrjola/diminish.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id diminish
                     :wait t :type git :protocol https :inherit t
                     :depth treeless :ref
                     "43d0b9e0d18e4d124e3f7dad9d73275fb678fa50"))
 (doom-themes :source "elpaca-menu-lock-file" :recipe
              (:package "doom-themes" :fetcher github :repo
                        "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el"
                                   "extensions/*.el")
                        :source "elpaca-menu-lock-file" :id
                        doom-themes :type git :protocol https :inherit
                        t :depth treeless :ref
                        "a59202912ad55014e53a685eee6cd94130bdd4fd"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source "Init file" :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "7763ede8babd238b7a9a5835833ef4f4bdc4e53e" :depth nil
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build
                               (:not elpaca-source elpaca-build-docs)
                               :source "elpaca-menu-lock-file" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "7763ede8babd238b7a9a5835833ef4f4bdc4e53e"))
 (elpy :source "elpaca-menu-lock-file" :recipe
       (:package "elpy" :fetcher github :repo "jorgenschaefer/elpy"
                 :files ("*.el" "NEWS.rst" "snippets" "elpy") :source
                 "elpaca-menu-lock-file" :id elpy :type git :protocol
                 https :inherit t :depth treeless :ref
                 "9cdf26dfea1cb044b3cf1dfa9755b6479bfd9a1c"))
 (elscreen :source "elpaca-menu-lock-file" :recipe
           (:package "elscreen" :repo "knu/elscreen" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id elscreen
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "cc58337faf5ba1eae7e87f75f6ff3758675688f2"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :source "elpaca-menu-lock-file" :id embark :type
                   git :protocol https :inherit t :depth treeless :ref
                   "87e53827cf6659dcc4ac4e54be9af34aeca44f6e"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark"
                           :fetcher github :files
                           ("embark-consult.el") :source
                           "elpaca-menu-lock-file" :id embark-consult
                           :type git :protocol https :inherit t :depth
                           treeless :ref
                           "87e53827cf6659dcc4ac4e54be9af34aeca44f6e"))
 (esh-autosuggest :source "elpaca-menu-lock-file" :recipe
                  (:package "esh-autosuggest" :fetcher sourcehut :repo
                            "dieggsy/esh-autosuggest" :old-names
                            (company-eshell-autosuggest) :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id
                            esh-autosuggest :type git :protocol https
                            :inherit t :depth treeless :ref
                            "40774022105ed16287fcf26553a16c4cdda5e1ab"))
 (eshell-up :source "elpaca-menu-lock-file" :recipe
            (:package "eshell-up" :fetcher github :repo
                      "peterwvj/eshell-up" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id eshell-up
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "1999afaa509204b780db44e99ac9648fe7d92d32"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id
                                 exec-path-from-shell :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "6146fdc16e9882df270be7e58ae8d628032d6bc4"))
 (expreg :source "elpaca-menu-lock-file" :recipe
         (:package "expreg" :repo
                   ("https://github.com/casouri/expreg.git" . "expreg")
                   :tar nil :host gnu :files ("*" (:exclude ".git"))
                   :source "elpaca-menu-lock-file" :id expreg :type
                   git :protocol https :inherit t :depth treeless :ref
                   "a2d8d7549bcf3de4fa204677b95af1ee5cc7c950"))
 (extmap :source "elpaca-menu-lock-file" :recipe
         (:package "extmap" :repo "doublep/extmap" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id extmap :type
                   git :protocol https :inherit t :depth treeless :ref
                   "bb4867e13ec7480d4dbc1d99888627e4cc8ef2f6"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id flycheck
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "0707e3426edf8dd9529bd8ad1058eb0b3add4507"))
 (flycheck-clj-kondo :source "elpaca-menu-lock-file" :recipe
                     (:package "flycheck-clj-kondo" :fetcher github
                               :repo "borkdude/flycheck-clj-kondo"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id
                               flycheck-clj-kondo :type git :protocol
                               https :inherit t :depth treeless :ref
                               "414a3ead1faefb234d658fd8a8ba121c95b71de2"))
 (fullframe :source "elpaca-menu-lock-file" :recipe
            (:package "fullframe" :fetcher sourcehut :repo
                      "tomterl/fullframe" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id fullframe
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "886b831c001b44ec95aec4ff36e8bc1b3003c786"))
 (git-auto-commit-mode :source "elpaca-menu-lock-file" :recipe
                       (:package "git-auto-commit-mode" :fetcher
                                 github :repo
                                 "ryuslash/git-auto-commit-mode"
                                 :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "elpaca-menu-lock-file" :id
                                 git-auto-commit-mode :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "a7b59acea622a737d23c783ce7d212fefb29f7e6"))
 (haml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "haml-mode" :repo "nex3/haml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id haml-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "3bb4a96535eb5c81dbe6a43bfa8d67a778d449c0"))
 (highlight-indentation :source "elpaca-menu-lock-file" :recipe
                        (:package "highlight-indentation" :repo
                                  "antonj/Highlight-Indentation-for-Emacs"
                                  :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :id
                                  highlight-indentation :type git
                                  :protocol https :inherit t :depth
                                  treeless :ref
                                  "d88db4248882da2d4316e76ed673b4ac1fa99ce3"))
 (highlight-symbol :source "elpaca-menu-lock-file" :recipe
                   (:package "highlight-symbol" :fetcher github :repo
                             "nschum/highlight-symbol.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id
                             highlight-symbol :type git :protocol
                             https :inherit t :depth treeless :ref
                             "7a789c779648c55b16e43278e51be5898c121b3a"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "elpaca-menu-lock-file" :id ht :type git
               :protocol https :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source
                  "elpaca-menu-lock-file" :id hydra :type git
                  :protocol https :inherit t :depth treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (indium :source "elpaca-menu-lock-file" :recipe
         (:package "indium" :fetcher github :repo
                   "NicolasPetton/Indium" :old-names (jade) :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id indium :type
                   git :protocol https :inherit t :depth treeless :ref
                   "8499e156bf7286846c3a2bf8c9e0c4d4f24b224c"))
 (js2-mode :source "elpaca-menu-lock-file" :recipe
           (:package "js2-mode" :repo "mooz/js2-mode" :fetcher github
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id js2-mode
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "41d0e7f5ef51109c682016baa6fc6846e03e8517"))
 (js2-refactor :source "elpaca-menu-lock-file" :recipe
               (:package "js2-refactor" :repo
                         "js-emacs/js2-refactor.el" :fetcher github
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id
                         js2-refactor :type git :protocol https
                         :inherit t :depth treeless :ref
                         "e1177c728ae52a5e67157fb18ee1409d8e95386a"))
 (json-process-client :source "elpaca-menu-lock-file" :recipe
                      (:package "json-process-client" :fetcher github
                                :repo
                                "DamienCassou/json-process-client"
                                :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "elpaca-menu-lock-file" :id
                                json-process-client :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "6485953fe6eff62938fd08720811c6fdd09d7d22"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source
                  "elpaca-menu-lock-file" :id llama :type git
                  :protocol https :inherit t :depth treeless :ref
                  "6850d0c91b629da14fdff2300c222289d1a0029a"))
 (logview :source "elpaca-menu-lock-file" :recipe
          (:package "logview" :repo "doublep/logview" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id logview :type
                    git :protocol https :inherit t :depth treeless
                    :ref "50cac3f726abd4e7872b8fa36ede72550ce66a64"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "elpaca-menu-lock-file" :id lv :type
               git :protocol https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("githooks" "githooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "elpaca-menu-lock-file" :id magit :type git
                  :protocol https :inherit t :depth treeless :ref
                  "afa6095d723c9557d781d648cc9b0b6ad624d9ca"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi")
                          :source "elpaca-menu-lock-file" :id
                          magit-section :type git :protocol https
                          :inherit t :depth treeless :ref
                          "afa6095d723c9557d781d648cc9b0b6ad624d9ca"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :fetcher github :repo
                       "minad/marginalia" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id marginalia
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "381af583d2530796b9955ff2fe0cba3ad3119943"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "elpaca-menu-lock-file" :id
                          markdown-mode :type git :protocol https
                          :inherit t :depth treeless :ref
                          "1d22840d588be29aa57eca36f28a32bede331550"))
 (multiple-cursors :source "elpaca-menu-lock-file" :recipe
                   (:package "multiple-cursors" :fetcher github :repo
                             "magnars/multiple-cursors.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id
                             multiple-cursors :type git :protocol
                             https :inherit t :depth treeless :ref
                             "6086e34e86cc9c305c693f440e648588335669c9"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id
                         no-littering :wait t :type git :protocol
                         https :inherit t :depth treeless :ref
                         "fe0adc238f3ddd92422013a3cc39af2733924208"))
 (npm-mode :source "elpaca-menu-lock-file" :recipe
           (:package "npm-mode" :fetcher github :repo
                     "mojochao/npm-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id npm-mode
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "3ee7c0bad5b7a041d4739ef3aaa06a3dc764e5eb"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id orderless
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "e271a8ec938929b726331236cf56d1c44571122e"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id paredit :type
                    git :protocol https :inherit t :depth treeless
                    :ref "af075775af91f2dbc63b915d762b4aec092946c4"))
 (parseclj :source "elpaca-menu-lock-file" :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id parseclj
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "249079da5c2655a8e503b33280c1ab1341e72de2"))
 (parseedn :source "elpaca-menu-lock-file" :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id parseedn
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "6ab98d33ae175f28e4bbf1df874030e1ff7b7add"))
 (pfuture :source "elpaca-menu-lock-file" :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture"
                    :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pfuture :type
                    git :protocol https :inherit t :depth treeless
                    :ref "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (platformio-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "platformio-mode" :repo
                            "ZachMassia/PlatformIO-Mode" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "elpaca-menu-lock-file" :id
                            platformio-mode :type git :protocol https
                            :inherit t :depth treeless :ref
                            "f4fd8932995a8aed80eab14e54232010c2889012"))
 (pos-tip :source "elpaca-menu-lock-file" :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "elpaca-menu-lock-file" :id pos-tip :type
                    git :protocol https :inherit t :depth treeless
                    :ref "4889e08cf9077c8589ea6fea4e2ce558614dfcde"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo
                     "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id posframe
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "1a6a25c58559ef43821a071c43b23de5cbc3cc3a"))
 (powerline :source "elpaca-menu-lock-file" :recipe
            (:package "powerline" :fetcher github :repo
                      "milkypostman/powerline" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id powerline
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "c35c35bdf5ce2d992882c1f06f0f078058870d4a"))
 (projectile :source "elpaca-menu-lock-file" :recipe
             (:package "projectile" :fetcher github :repo
                       "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "elpaca-menu-lock-file" :id projectile
                       :type git :protocol https :inherit t :depth
                       treeless :ref
                       "4d2be8680fe445c4ca88177f0ed838051cadfe58"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
         (:package "pyvenv" :fetcher github :repo
                   "jorgenschaefer/pyvenv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id pyvenv :type
                   git :protocol https :inherit t :depth treeless :ref
                   "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
                  :tar nil :host gnu :branch "externals/queue" :files
                  ("*" (:exclude ".git")) :source
                  "elpaca-menu-lock-file" :id queue :type git
                  :protocol https :inherit t :depth treeless :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "elpaca-menu-lock-file" :id
                               rainbow-delimiters :type git :protocol
                               https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (rainbow-mode :source "elpaca-menu-lock-file" :recipe
               (:package "rainbow-mode" :repo
                         ("https://github.com/emacsmirror/gnu_elpa"
                          . "rainbow-mode")
                         :tar nil :host gnu :branch
                         "externals/rainbow-mode" :files
                         ("*" (:exclude ".git")) :source
                         "elpaca-menu-lock-file" :id rainbow-mode
                         :type git :protocol https :inherit t :depth
                         treeless :ref
                         "9d333d3a92132c2a9057d2a39e1ebd8cc575a1bb"))
 (restclient :source "elpaca-menu-lock-file" :recipe
             (:package "restclient" :fetcher github :repo
                       "emacsorphanage/restclient" :files
                       ("restclient.el") :source
                       "elpaca-menu-lock-file" :id restclient :type
                       git :protocol https :inherit t :depth treeless
                       :ref "d280632df39a175dac06037038105e32945624be"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "elpaca-menu-lock-file" :id s :type git
              :protocol https :inherit t :depth treeless :ref
              "d7c04b84d03481a1ed62ee13dbe595224ccbe57c"))
 (sass-mode :source "elpaca-menu-lock-file" :recipe
            (:package "sass-mode" :repo "nex3/sass-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id sass-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "247a0d4b509f10b28e4687cd8763492bca03599b"))
 (scss-mode :source "elpaca-menu-lock-file" :recipe
            (:package "scss-mode" :repo "antonj/scss-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id scss-mode
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "cf58dbec5394280503eb5502938f3b5445d1b53d"))
 (sesman :source "elpaca-menu-lock-file" :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "elpaca-menu-lock-file" :id sesman :type
                   git :protocol https :inherit t :depth treeless :ref
                   "7eb733acb33e610a53979fa7fc13393eeda3cc53"))
 (simple-bookmarks :source "elpaca-menu-lock-file" :recipe
                   (:package "simple-bookmarks" :fetcher github :repo
                             "jtkDvlp/simple-bookmarks" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "elpaca-menu-lock-file" :id
                             simple-bookmarks :host github :type git
                             :protocol https :inherit t :depth
                             treeless :ref
                             "54e8d771bcdb0eb235b31c0aa9642171369500e5"))
 (simple-httpd :source "elpaca-menu-lock-file" :recipe
               (:package "simple-httpd" :repo
                         "skeeto/emacs-web-server" :fetcher github
                         :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "elpaca-menu-lock-file" :id
                         simple-httpd :type git :protocol https
                         :inherit t :depth treeless :ref
                         "0c17124cefddf2d50defcec5f2bd25fa6d1db756"))
 (smartparens :source "elpaca-menu-lock-file" :recipe
              (:package "smartparens" :fetcher github :repo
                        "Fuco1/smartparens" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id
                        smartparens :type git :protocol https :inherit
                        t :depth treeless :ref
                        "82d2cf084a19b0c2c3812e0550721f8a61996056"))
 (spaceline :source "elpaca-menu-lock-file" :recipe
            (:package "spaceline" :fetcher github :repo
                      "TheBB/spaceline" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "elpaca-menu-lock-file" :id spaceline
                      :type git :protocol https :inherit t :depth
                      treeless :ref
                      "086420d16e526c79b67fc1edec4c2ae1e699f372"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :tar nil :host gnu :files ("*" (:exclude ".git"))
                    :source "elpaca-menu-lock-file" :id spinner :type
                    git :protocol https :inherit t :depth treeless
                    :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (tide :source "elpaca-menu-lock-file" :recipe
       (:package "tide" :fetcher github :repo "ananthakumaran/tide"
                 :files (:defaults "tsserver") :source
                 "elpaca-menu-lock-file" :id tide :type git :protocol
                 https :inherit t :depth treeless :ref
                 "9498c4c7fc97d8042fdff532f47f1dc79ebd163a"))
 (treemacs :source "elpaca-menu-lock-file" :recipe
           (:package "treemacs" :fetcher github :repo
                     "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons"
                                "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :source "elpaca-menu-lock-file" :id treemacs
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-icons-dired :source "elpaca-menu-lock-file" :recipe
                       (:package "treemacs-icons-dired" :fetcher
                                 github :repo
                                 "Alexander-Miller/treemacs" :files
                                 ("src/extra/treemacs-icons-dired.el")
                                 :source "elpaca-menu-lock-file" :id
                                 treemacs-icons-dired :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (treemacs-magit :source "elpaca-menu-lock-file" :recipe
                 (:package "treemacs-magit" :fetcher github :repo
                           "Alexander-Miller/treemacs" :files
                           ("src/extra/treemacs-magit.el") :source
                           "elpaca-menu-lock-file" :id treemacs-magit
                           :type git :protocol https :inherit t :depth
                           treeless :ref
                           "2ab5a3c89fa01bbbd99de9b8986908b2bc5a7b49"))
 (undo-tree :source "elpaca-menu-lock-file" :recipe
            (:package "undo-tree" :repo
                      ("https://gitlab.com/tsc25/undo-tree"
                       . "undo-tree")
                      :tar nil :host gnu :files
                      ("*" (:exclude ".git")) :source
                      "elpaca-menu-lock-file" :id undo-tree :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2bf5e230f1d11df7bbd9d8c722749e34482bc458"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "elpaca-menu-lock-file" :id vertico
                    :type git :protocol https :inherit t :depth
                    treeless :ref
                    "c60d80215d19b4b36c0ba49599b6815ed35f2ac1"))
 (web-mode :source "elpaca-menu-lock-file" :recipe
           (:package "web-mode" :repo "fxbois/web-mode" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id web-mode
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "ce24723eb900c455b488d224910519bd36af580a"))
 (web-mode-edit-element :source "elpaca-menu-lock-file" :recipe
                        (:package "web-mode-edit-element" :fetcher
                                  github :repo
                                  "jtkDvlp/web-mode-edit-element"
                                  :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "elpaca-menu-lock-file" :id
                                  web-mode-edit-element :host github
                                  :type git :protocol https :inherit t
                                  :depth treeless :ref
                                  "a3737f9d4f01baa449d68998bfd35c608777256e"))
 (with-editor :source "elpaca-menu-lock-file" :recipe
              (:package "with-editor" :fetcher github :repo
                        "magit/with-editor" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "elpaca-menu-lock-file" :id
                        with-editor :type git :protocol https :inherit
                        t :depth treeless :ref
                        "4d887efc750b7a69554dbd2d0c79ca447f8f08f3"))
 (xref-js2 :source "elpaca-menu-lock-file" :recipe
           (:package "xref-js2" :fetcher github :repo
                     "js-emacs/xref-js2" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "elpaca-menu-lock-file" :id xref-js2
                     :type git :protocol https :inherit t :depth
                     treeless :ref
                     "e215af9eedac69b40942fff9d5514704f9f4d43e"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :fetcher github :repo
                      "joaotavora/yasnippet" :files
                      (:defaults ("doc" "doc/*.org")) :source
                      "elpaca-menu-lock-file" :id yasnippet :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (yasnippet-snippets :source "elpaca-menu-lock-file" :recipe
                     (:package "yasnippet-snippets" :repo
                               "AndreaCrotti/yasnippet-snippets"
                               :fetcher github :files
                               ("*.el" "snippets" ".nosearch") :source
                               "elpaca-menu-lock-file" :id
                               yasnippet-snippets :type git :protocol
                               https :inherit t :depth treeless :ref
                               "606ee926df6839243098de6d71332a697518cb86"))
 (ztree :source "elpaca-menu-lock-file" :recipe
        (:package "ztree" :fetcher codeberg :repo "fourier/ztree"
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "elpaca-menu-lock-file" :id ztree :type git
                  :protocol https :inherit t :depth treeless :ref
                  "48f17807ac6c765a66cf29b2fcdd08f1aa7da7d6")))
