;;; init-logview.el --- Log files -*- lexical-binding: t; -*-

;;; Commentary:

;; logview with submodes for the own log formats and keys for
;; filtering and navigating entries.

;;; Code:

(use-package
  logview
  :mode (("\\.log\\'" . logview-mode))
  :config
  (custom-set-variables
   '(logview-additional-submodes
     '(("electron"
        (format . "TIMESTAMP LEVEL [NAME] - MESSAGE")
        (levels . "SLF4J")
        (timestamp . ("DateTime(ms) with zone"))
        (aliases))
       ("web-main"
        (format . "TIMESTAMP IGNORED LEVEL [NAME] <THREAD> - MESSAGE")
        (levels . "SLF4J")
        (timestamp . ("DateTime(ms) with zone"))
        (aliases))))
   '(logview-additional-timestamp-formats
     '(("DateTime(ms) with zone" . ((java-pattern . "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))))))

  :bind*
  (:map logview-mode-map
        ("C-c C-c" . logview-choose-submode)

        ("C-<left>" . logview-previous-entry)
        ("C-<right>" . logview-next-entry)
        ("C-<up>" . logview-go-to-message-beginning)
        ("G" . logview-last-entry)

        ("<tab>" . logview-toggle-entry-details)
        ("S-<tab>" . logview-toggle-details-globally)

        ("C--" . nil)
        ("C-- s" . logview-narrow-to-section)
        ("C-- t" . logview-narrow-to-thread)
        ("C-+" . logview-widen)

        ("f" . nil)
        ("F" . nil)
        ("f v" . logview-switch-to-view)
        ("f V" . logview-reset-all-filters)

        ("f s" . logview-save-filters-as-view-for-submode)
        ("f S" . logview-save-filters-as-global-view)
        ("f e" . logview-edit-filters)
        ("f k" . logview-delete-view)

        ("f l e" . logview-show-only-errors)
        ("f l w" . logview-show-errors-and-warnings)
        ("f l i" . logview-show-errors-warnings-and-information)
        ("f l d" . logview-show-errors-warnings-information-and-debug)
        ("f L" . logview-show-all-levels)
        ("F L" . logview-reset-level-filters)

        ;; NOTE: In `logview-edit-filters' a name filter is written with "a",
        ;; hence the second key.
        ("f n" . logview-add-include-name-filter)
        ("f a" . logview-add-include-name-filter)
        ("f N" . logview-add-exclude-name-filter)
        ("f A" . logview-add-exclude-name-filter)
        ("F N" . logview-reset-name-filters)
        ("F A" . logview-reset-name-filters)

        ("f t" . logview-add-include-thread-filter)
        ("f T" . logview-add-exclude-thread-filter)
        ("F T" . logview-reset-thread-filters)

        ("f m" . logview-add-include-message-filter)
        ("f M" . logview-add-exclude-message-filter)
        ("F M" . logview-reset-message-filters)

        ("v" . nil)
        ("v v" . logview-switch-to-view)
        ("v V" . logview-reset-all-filters)

        ("v s" . logview-save-filters-as-view-for-submode)
        ("v S" . logview-save-filters-as-global-view)
        ("v e" . logview-edit-submode-views)
        ("v E" . logview-edit-all-views)
        ("v k" . logview-delete-view)

        ("C-M-<left>" . logview-previous-section)
        ("C-M-<up>" . logview-go-to-section-beginning)
        ("C-M-<right>" . logview-next-section)
        ("C-M-<down>" . logview-go-to-section-end)

        ("s" . logview-set-section-view)

        ("t" . nil)
        ("t t" . logview-difference-to-current-entry)
        ("t T" . logview-forget-difference-base-entries)
        ("t j" . logview-go-to-difference-base-entry)))

(provide 'init-logview)

;;; init-logview.el ends here
