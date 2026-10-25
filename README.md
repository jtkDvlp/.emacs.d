# .emacs.d

Personal Emacs configuration for the latest stable Emacs on macOS.

## Layout

- `early-init.el`, `init.el`: entry points; `init.el` only loads the
  modules.
- `modules/`: one module per topic (`init-editing.el`, `init-vc.el`, …)
  and per language (`init-lang-clojure.el`, …); each file says in its
  header what belongs in it.
- `lisp/`: own libraries, prefixed `jtk-`.
- `etc/`: hand-kept files packages read (snippets, eshell aliases) and
  `elpaca-lock.eld`, which pins every package to a commit.
- `var/`, `elpaca/`: generated, not versioned.
- `scripts/check`: loads and byte-compiles the configuration in batch
  mode; CI runs it on every push.

## keymap system / rules
- there are global mappings for global commands to manage files, organize your work and better the user interface (ivy, winner, ace, whick-key ...)
- dont override global bindings, override its commands if there is a better one by the activated major mode
- bindings for navigating and handling code follow common rules tuned to respective language / major mode

## todo
- support language php
- support language typescript
