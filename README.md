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

## External tools

Installed outside Emacs; each is optional, and without it the feature it
serves is simply missing.

| Tool | Install | Used for |
|---|---|---|
| Xcode command line tools | `xcode-select --install` | compiling tree-sitter grammars, which Emacs offers to install on the first file of a kind |
| clojure-lsp | `brew install clojure-lsp/brew/clojure-lsp-native` | Clojure and ClojureScript: findings (clj-kondo), references, renaming, code actions, reference counts |
| typescript 7 or newer | `brew install typescript` | JavaScript through eglot (`tsc --lsp --stdio`); without it, JavaScript has no language server |
| lemminx | binary `lemminx-osx-…` from the [vscode-xml releases](https://github.com/redhat-developer/vscode-xml/releases), on `PATH`; `xattr -d com.apple.quarantine lemminx` once | XML: validation and completion from XML Schema and DTDs, formatting |
| xmllint | comes with macOS | formatting XML where lemminx does not run (`M-r =`) |
| shellcheck | `brew install shellcheck` | findings in shell scripts (flymake) |
| direnv | `brew install direnv` | per-project environment from `.envrc`, e.g. the Node version (`use nvm`, `layout node`) |
| Leiningen, Clojure CLI | `brew install leiningen clojure` | CIDER's jack-in |
| node, npm | via nvm or `brew install node` | the npm commands under `C-c M-j` |
| multimarkdown | `brew install multimarkdown` | markdown-mode's preview |
| a Nerd Font | installed by itself on the first start in the GUI (by hand: `M-x nerd-icons-install-fonts`) | the icons of the mode line and the start page |
| babashka | `brew install borkdude/brew/babashka` | `bbe`/`clje` in eshell: Clojure on the command line, also on data from remote hosts; tasks of `bb.edn` under `C-c M-j`; the scratch REPL on `M-! b` |

## Per-project environment (direnv)

`envrc` gives the buffers of a project the environment of its `.envrc`,
so shells, `C-c M-j` (npm) and language servers see the project's Node
version and variables. Set up once:

```bash
brew install direnv
# for the terminal, in ~/.zshrc (Emacs does not need it):
eval "$(direnv hook zsh)"
```

and, for Node from nvm, in `~/.config/direnv/direnvrc`:

```bash
export NODE_VERSIONS="$HOME/.nvm/versions/node"
export NODE_VERSION_PREFIX="v"
```

Per project, a `.envrc` in its root:

```bash
use node      # version from .nvmrc or .node-version, or e.g. "use node 20"
layout node   # ./node_modules/.bin on the PATH
# export API_URL=http://localhost:3000   # variables for this project only
# dotenv                                 # read a .env file as well
```

then `direnv allow` in that directory, again after every change to it.
`node --version` in eshell (`M-! s s`) in a file of the project shows
the version the `.envrc` names.

## Clojure in eshell (Babashka)

`bbe` (Babashka eval), or `clje` (Clojure eval), evaluates Clojure with
Babashka, on this machine. Input from a pipe is `*input*`, a sequence of
lines; a sequence as result prints one element per line:

```
λ bbe '(+ 1 2)'
3
λ seq 1 4 | bbe '(map #(* 2 (parse-long %)) *input*)'
2
4
6
8
```

On a remote directory (`cd /ssh:server:/var/log`), programs run on the
server, but `bbe` still runs here, so the server needs no Babashka. Its
data comes through the pipe, or as file names after the form, which
`bbe` copies here first and passes as `*command-line-args*`:

```
λ cat app.log | bbe '(filter #(str/includes? % "ERROR") *input*)'
λ bbe '(count (str/split-lines (slurp (first *command-line-args*))))' app.log
```

Inside the form, `TAB` completes Clojure's names, with Babashka's
aliases (`str/`, `fs/`, `json/`, ...). With point on `bbe` or `clje`
or in its form, `C-o` edits the form in a Clojure buffer, with
highlighting and structural editing, also where `bbe` stands in a pipe
(`seq 1 3 | clje '...' | sort`); `C-o` or `C-c C-c` puts it back
quoted, `C-c C-k` drops it. With point anywhere else, `C-o` inserts a
buffer's name, as eshell's `C-c M-b`.
In eshell's single quotes, `''` is one `'`.

The piped input is `*input*`, a lazy sequence of its lines. For the
whole text, `(str/join "\n" *input*)`; for EDN or JSON, read that, as in
`(edn/read-string (str/join "\n" *input*))`.

Other file names inside the code are local. Babashka with all its
options is `bb` (on the server, if it has one) or `/local:bb` (here).

`M-! b` opens a scratch buffer with a Babashka REPL, outside any
project; in a project with a `bb.edn`, `C-c M-j t` runs one of its
tasks.

## zsh in Emacs (mistty)

`M-! s z` opens zsh in mistty, in the project's root when there is one.
It is the real zsh with your `~/.zshrc` (nvm, direnv, plugins), but its
command line is edited with Emacs' keys, and earlier output is ordinary
text. Programs that need the full screen (`top`, `less`) take over the
terminal while they run. `M-! s t` is zsh in a plain terminal (eat),
`M-! s s` eshell.

Optional, once per mistty version: `M-x mistty-install` and "Download"
fetch the faster terminal (alacritty) for mistty; without it, mistty
uses Emacs' own. After an update of mistty, it falls back until fetched
again.

So that Emacs follows `cd` (for `C-x C-f` from the shell's directory),
add to `~/.zshrc`:

```zsh
function osc7_precmd() {
  printf "\e]7;file://%s%s\e\\\\" "$HOSTNAME" "$PWD"
}
precmd_functions+=(osc7_precmd)
```

In mistty, `C-e` takes a zsh autosuggestion, and `S-<return>` adds a
line to the command without running it.

## Colors in eshell

The input is highlighted while typing: commands, Lisp functions,
aliases, options, strings, variables and files, unknown commands in a
face of their own (eshell-syntax-highlighting; not on a server, where
each check would cost a round trip). Programs see
`TERM=dumb-emacs-ansi`, a terminal that shows colors but cannot move
the cursor, so git, grep and others color their output again.

## Servers (TRAMP)

Files, directories and shells on a server open like local ones, with
`C-x C-f /remote:user@host:/path`, in dired, and in eshell with
`cd /remote:user@host:/path`. `/remote:` is the preferred method under
a name of its own, today scp: it logs in with ssh and copies files above
10 KB with scp. `/ssh:` works as well, but sends files through the
login shell, which is slower for large ones.

- In dired with a local directory in one window and a server's in the
  other, `C` copies and `R` moves to the other window's directory, which
  replaces calling scp by hand.
- `M-! s s` in a buffer on a server opens eshell there; programs run on
  the server, `bbe` here (see above). `M-! s z` starts the server's own
  shell in mistty.
- In eshell on a server, `cd` stays there as in a shell on the server:
  `cd /var/log`, `cd ~` and `cd` alone are the server's. `cd /local:DIR`
  goes to `DIR` on this machine, `cd /local:` to its home directory.
  Other commands keep eshell's rule: an absolute file name in a Lisp
  command such as `ls` or `cat` means this machine.
- Full-screen programs such as `htop` run on the server in eat, which
  announces itself there as `xterm-256color`.
- Programs start through ssh directly, over the connection TRAMP keeps
  open. The first access to a server asks for a password or passphrase
  in the minibuffer, if the login needs one; programs then start without
  asking. Has the connection closed, TRAMP opens it again first and asks
  once more. Over several hops TRAMP uses its slower way by itself.
- Files on a server get no version control in the mode line and no
  lock files; magit works there all the same.

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
- Each kind of project (Clojure, Babashka, npm) brings its commands, under
  `C-c M-j` in every buffer of the project and under `j` in the project
  commands.
- `M-X` lists the commands that fit the current buffer, with their
  keys; `C-h b` lists all keys of the buffer to search (with `C-u` the
  global ones too).

Same purpose, same key, in every language that has it:

| Purpose | Key |
|---|---|
| move along / into / out of the structure (s-expressions, elements) | `C-<left>` `C-<right>` `C-<down>` `C-<up>` |
| change the structure (slurp, barf, raise, …) | `M-<arrows>`, `C-M-<arrows>`; see `init-structural-editing` |
| jump to definition / references / line; search names | `M-j d` / `M-j r` / `M-j l`; `M-j s`; `M-J` goes back |
| documentation at point | `C-h h` |
| rename / code actions / organise imports / format | `M-r r` / `M-r a` / `M-r o` / `M-r =` |
| evaluate the form at point / the last one / the buffer | `C-c C-c` / `C-M-x` / `C-c M-k` (Markdown: render) |
| commands of the project's kind (REPL, bb tasks, npm) | `C-c M-j`, in every buffer of the project |
| next / previous finding, list them | `C-c ! n` / `C-c ! p`, `C-c ! l` |
| fold the block whose first line point is on / all blocks | `TAB` within the line's indentation / `S-TAB` |
| narrow to the region or defun / widen | `C--` / `C-+` |
| comment | `C-;` |

| Prefix | Keymap | Module | For |
|---|---|---|---|
| `M--` | `jtk-bookmarks-map` | `init-bookmarks` | bookmarks of files, directories, MySQL connections |
| `M-c` | `jtk-multiple-cursors-map` | `init-editing` | multiple cursors |
| `M-j` | `jtk-jump-map` | `init-editing`, languages | definitions (`d`), references (`r`), lines (`l`), keywords like `TODO` or `WATCHOUT` (`t`, then `n` and `p` go on), in Clojure apropos (`s`); `M-J` goes back |
| `M-r` where a language server runs | `eglot-mode-map` | `init-lsp` | renaming (`r`), code actions (`a`), organising imports (`o`), formatting (`=`) |
| `M-m` | `jtk-highlight-symbol-map` | `init-editing` | next, previous, replace symbol (repeats); `C-m` marks it |
| `M-!` | `jtk-run-map` | `init-keys` | `m` keyboard macros (`jtk-macro-map`), `s` shells (`jtk-shell-map`), `b` a Babashka REPL with a scratch buffer |
| `M-+` | `jtk-view-map` | `init-keys` | `f` font size (`jtk-zoom-map`), `w` frame size (`jtk-frame-map`); both repeat |
| `M-'` | `jtk-tab-map` | `init-windows` | tabs: Emacs' `C-x t`, plus a tab for the current project (`c`), switching (`n`, `p`, repeat), closing with its buffers (`K`) |
| `C-M-'` | `jtk-buffer-move-map` | `init-windows` | moving the buffer to a neighbouring window |
| `M-v` | `jtk-vc-map` | `init-vc` | git |
| `C-x x` | `ctl-x-x-map` (Emacs) | `init-project`, `init-treemacs` | switching projects (`f`), file tree (`d`) |
| `C-x x p` | `jtk-project-map` | `init-project`, languages | project commands: Emacs' `C-x p` with consult for buffers (`b`) and grep (`g`), Magit (`v`, `m`), commands of the project's kinds (`j`) |
| `C-c M-j` in a project of a known kind | `jtk-project-run-kind-command`, in Clojure buffers `jtk-clojure-project-map` | `init-project`, languages | starting or connecting a REPL; a task of `bb.edn` (`t`); npm install, run, update |
| `C-a` in HTML | `jtk-html-attribute-map` | `init-lang-html` | editing attributes (repeats) |

