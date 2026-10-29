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

## Keys

- Global keys are for global tasks: files, buffers, windows, search,
  organising work. Modes do not override them; where a mode has a
  better command for the same task, it binds it to the same key.
- Keys for moving through and editing code follow one scheme across
  languages (`init-structural-editing`); a language names only where
  it differs. The same holds for jumping (`M-j`, through xref) and for
  the documentation at point (`C-h h`, through eldoc).
- Every global prefix is a named keymap of the module that owns it.
  C-h after a prefix lists its keys; `M-x describe-keymap` shows a map.
- Keys of a repeating prefix work alone after the first one, until
  another key is typed (`repeat-mode`): `M-+ f + + -`. Emacs' own
  repeat maps work too, e.g. `C-x o o`.
- In a list, RET does the usual thing with the marked entry and `M-x`
  (embark; x for execute) offers the others; C-h after `M-x` lists them.
  On a project in the project list (`C-x x f`), `M-x` offers the project
  commands of `C-x x p` for that project.
- Every list that C-h opens is searchable and shows the keys; `@`
  followed by a key runs that key's command directly.
- A list exported with `C-o` into a buffer of its own (projects, for
  instance) takes the same action keys directly on the entry under the
  cursor: `e` on a project opens eshell there; `M-a` turns that off.
- Each kind of project (Clojure, npm) brings its commands,
  under `C-c M-j` in its buffers and under `j` in the project commands.
- `M-X` lists the commands that fit the current buffer, with their
  keys; `C-h b` lists all keys of the buffer to search (with `C-u` the
  global ones too).

| Prefix | Keymap | Module | For |
|---|---|---|---|
| `M--` | `jtk-bookmarks-map` | `init-bookmarks` | bookmarks of files, directories, MySQL connections |
| `M-c` | `jtk-multiple-cursors-map` | `init-editing` | multiple cursors |
| `M-j` | `jtk-jump-map` | `init-editing`, languages | definitions (`d`), references (`r`), lines (`l`); `M-J` goes back |
| `M-m` | `jtk-highlight-symbol-map` | `init-editing` | next, previous, replace symbol (repeats); `C-m` marks it |
| `M-!` | `jtk-run-map` | `init-keys` | `m` keyboard macros (`jtk-macro-map`), `s` shells (`jtk-shell-map`) |
| `M-+` | `jtk-view-map` | `init-keys` | `f` font size (`jtk-zoom-map`), `w` frame size (`jtk-frame-map`); both repeat |
| `M-'` | `jtk-tab-map` | `init-windows` | tabs: Emacs' `C-x t`, plus a tab for the current project (`c`), switching (`n`, `p`, repeat), closing with its buffers (`K`) |
| `C-M-'` | `jtk-buffer-move-map` | `init-windows` | moving the buffer to a neighbouring window |
| `M-v` | `jtk-vc-map` | `init-vc` | git |
| `C-M--` | `jtk-org-map` | `init-org` | capturing, agenda |
| `C-x x` | `ctl-x-x-map` (Emacs) | `init-project`, `init-treemacs` | switching projects (`f`), file tree (`d`) |
| `C-x x p` | `jtk-project-map` | `init-project`, languages | project commands: Emacs' `C-x p` with consult for buffers (`b`) and grep (`g`), Magit (`v`, `m`), commands of the project's kinds (`j`) |
| `C-c M-j` in a project kind's buffers | `jtk-clojure-project-map`, npm-mode's | languages | starting or connecting a REPL, npm |
| `C-a` in HTML | `jtk-html-attribute-map` | `init-lang-html` | editing attributes (repeats) |

