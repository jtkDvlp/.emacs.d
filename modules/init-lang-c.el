;;; init-lang-c.el --- C++ -*- lexical-binding: t; -*-

;;; Commentary:

;; c++-mode, also for Arduino sketches, and PlatformIO.

;;; Code:

(require 'jtk-project)

(use-package
  c++-mode
  :ensure nil

  :mode
  (("\\.ino\\'" . c++-mode)
   ("\\.h\\'" . c++-mode)
   ("\\.cpp\\'" . c++-mode))

  :hook
  ((c++-mode . (lambda () (rainbow-mode 0)))))

(use-package
  platformio-mode

  :mode
  (("platformio\\.ini\\'" . platformio-mode))

  :hook
  ((c++-mode . platformio-mode))

  :init
  ;; NOTE: platformio-mode's own keys (build b, upload u, ...), under
  ;; C-c M-j as well as its C-c i, and under j in the project commands.
  (add-to-list 'jtk-project-kinds
               '("PlatformIO" ("platformio.ini") platformio-command-map
                 platformio-mode)
               t)

  :config
  (keymap-set platformio-mode-map "C-c M-j" 'platformio-command-map))

(provide 'init-lang-c)

;;; init-lang-c.el ends here
