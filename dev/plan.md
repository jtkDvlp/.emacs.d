# Umbau der Emacs-Konfiguration – Befund und Plan

Stand: 2026-10-21. Grundlage ist der Commit `a116aa1` („Some settings“, März
2025). Alles hier ist **Lesart des Codes** — in der Umgebung, in der der
Plan entstand, läuft kein Emacs. Was als Fehler markiert ist, wurde nicht
beobachtet, sondern aus dem Code geschlossen.

Inhalt:

1. Bestandsaufnahme
2. Befunde zur Grundstruktur (Dateiablage, Dateiaufbau, use-package,
   Paketverwaltung)
3. Nebenbei gefundene Fehler
4. Zielbild
5. Plan: Phasen und Reihenfolge des Paket-Reviews
6. Paketinventar
7. Offene Fragen

---

## 1. Bestandsaufnahme

28 Dateien, rund 2 800 Zeilen Elisp, etwa 60 Pakete (davon rund 45 von
MELPA/ELPA, der Rest eingebaut). Die Konfiguration ist über Jahre
gewachsen (erste Commits 2020) und folgt einem erkennbaren Konzept:

- **Ein eigenes Tastaturkonzept** (README): globale Präfixe je Aufgabe
  (`M-'` Bildschirme, `M-v` Versionskontrolle, `M-c` Mehrfach-Cursor,
  `M-- ` Lesezeichen, `C-M--` Org-Erfassung, `M-j` Springen), und eine
  **sprachübergreifend gleiche Belegung für strukturelles Editieren**
  (`C-<Pfeil>` bewegen, `M-<Pfeil>` slurp/barf/raise, `C-M-<Pfeil>`
  vertauschen, `C-k`/`M-k` löschen) — in Clojure, Elisp, Python, JS, TS
  und HTML jeweils sinngemäß gleich.
- **Eigene Werkzeuge**: ein IDE-Fensterlayout über `display-buffer-alist`
  (`pkg--ide`), Fenster-Spiegelung (`pkg--window-mirror`), Zoom, Narrow,
  ein umfangreicher CIDER-Aufsatz (Leiningen-Profile, figwheel-main,
  System-Start/-Stop über `user/system-go!`), Gerrit-Push in magit.
- **Technische Basis**: package.el + use-package, ivy/counsel, company,
  flycheck, projectile, smartparens, spaceline, elscreen.

Diese Basis war um 2018–2020 der Stand der Dinge. Seitdem hat sich das
Ökosystem deutlich bewegt — vor allem durch Funktionen, die **in Emacs
selbst** gelandet sind:

| Emacs-Version | eingebaut seitdem | ersetzt hier (Kandidat) |
|---|---|---|
| 26.1 | `display-line-numbers-mode` | `linum` (seit 29.1 obsolet) |
| 27.1 | `early-init.el`, `tab-bar-mode`, `windmove-swap-states-*` | `elscreen`, `buffer-move`, UI-Abschaltung in `init` |
| 28.1 | `repeat-mode`, `use-short-answers`, `undo-redo`, ausgereiftes `project.el` | `smartrep`, `defalias yes-or-no-p`, Teile von `undo-tree`, ggf. `projectile` |
| 29.1 | `use-package`, `eglot` (LSP), `treesit` (tree-sitter), `setopt`, `--init-directory`, `package-vc` | `tide`, `elpy`, `xref-js2`, `js2-mode`, … ; `custom-set-variables` im Code |
| 30.1 | `which-key`, `use-package :vc`, `completion-preview-mode` | `which-key` von MELPA |

Daraus folgt die Leitlinie für „state of the art“ in einem Satz:
**Pakete bevorzugen, die auf Emacs' Standard-Schnittstellen aufsetzen** —
`completing-read`, `completion-at-point`, `xref`, `flymake`, `project`,
`eldoc`, `display-buffer-alist`. Solche Pakete sind gegeneinander
austauschbar und brauchen keinen Klebstoff. Die heutige Konfiguration hat
viel Klebstoff, gerade weil ivy, company, flycheck und projectile je eine
eigene Welt mitbringen (`projectile-completion-system 'ivy`,
`magit-completing-read-function 'ivy-completing-read`,
`counsel-esh-history`, `company-php`-Backend, …).

---

## 2. Befunde zur Grundstruktur

Nummeriert, damit wir uns darauf beziehen können: **S** = Dateiablage,
**F** = Aufbau der Dateien, **U** = use-package, **P** = Paketverwaltung.

### Dateiablage

**S1 – Es gibt kein `early-init.el`.** Menü-, Werkzeug- und Scrollleiste
werden erst abgeschaltet, wenn `base--apperance` geladen ist, und das Fenster
wird erst am Ende maximiert — der erste Frame flackert sichtbar. Außerdem
ist `package-enable-at-startup nil` in `configuration.el` wirkungslos: Seit
Emacs 27 hat Emacs die Pakete zu diesem Zeitpunkt längst aktiviert, und das
anschließende `package-initialize` aktiviert sie ein zweites Mal.

**S2 – Konfiguration und eigene Bibliotheken liegen gemischt.**
`configuration/` enthält zwei Arten Datei: Module mit Seiteneffekten beim
Laden (Variablen setzen, Modi einschalten, Tasten binden) und Bibliotheken
ohne solche (`pkg--ide`, `pkg--zoom`, …), die nur Funktionen bereitstellen.
Die Trennung ist real — eine Bibliothek ließe sich als Paket herauslösen,
wie es mit `simple-bookmarks` und `web-mode-edit-element` offenbar schon
geschehen ist —, aber sie ist nur am Präfix ablesbar.

**S3 – Die Präfixe sagen das Falsche.** In Elisp bedeutet `--` im Namen
„intern“ (`paket--hilfsfunktion`); `base--apperance` liest sich als
„interne Funktion `apperance` des Pakets `base`“. Ein Präfix ist nötig —
Feature-Namen teilen sich einen globalen Namensraum, `(provide 'completion)`
würde kollidieren —, aber er sollte sprechend sein. Und `base` trennt
nichts: Alles, was keine Sprache ist, ist `base`. (Nebenbei: „apperance“
statt „appearance“.)

**S4 – Sammelbecken statt Themen.** Die Richtlinie verlangt Gliederung nach
Thema, nicht nach Schicht oder Gefühl. Heute:

| Datei | Zeilen | enthält u. a. |
|---|---:|---|
| `base--interface.el` | 296 | Vervollständigung (ivy, counsel, swiper), Fenster (winner, fullframe, buffer-move, elscreen, ace-window), Vergleich (ediff, ztree), Springen (ace-jump), Editieren (expand-region, undo-tree, multiple-cursors), Tastenhilfe (which-key), Zoom, Makros |
| `base--development.el` | 344 | Klammern, Einrückung, Vervollständigung im Puffer, Git, Syntaxprüfung, Snippets, Symbol-Hervorhebung, REST-Client, SQL, HTTP-Server, Markdown, Log-Betrachter, IDE-Layout |
| `base--filemanagement.el` | 204 | Projekte, Dateibaum, Lesezeichen (mit MySQL-Verbindungen), Auto-Commit, Zeichenkodierung, Auto-Revert, eine `zsh`-Funktion |

Wer etwa die Vervollständigung umbaut, sucht heute in vier Dateien
(`interface`, `filemanagement`, `development`, `shell`).

**S5 – Erzeugtes liegt verstreut.** `.gitignore` zählt zehn Einzeldateien
auf (`recentf`, `projectile.cache`, `bookmarks`, …), `.mc-lists.el` liegt
im Wurzelverzeichnis, und jedes neue Paket legt seine Datei irgendwo dazu.
`custom.el` ist versioniert **und** in `.gitignore` — eine leere Datei im
Repo, deren Änderungen niemand sieht. Das ist das Emacs-Gegenstück zur
Regel „`target/` ist jederzeit löschbar“: Es gibt kein `target/`.

**S6 – Maschinenspezifisches steht im versionierten Code.** `init.el` setzt
`env-jira-host-url`. Das gehört in eine lokale, nicht versionierte Datei.

**S7 – Was schon stimmt:** `snippets/` und `eshell/alias` sind
handgeschrieben und versioniert — das ist die Rolle von `resources/`. Das
bleibt so.

### Aufbau der Dateien

**F1 – Kein Dateikopf, kein `lexical-binding`.** Keine Datei beginnt mit
`;;; name.el --- Zweck -*- lexical-binding: t; -*-`. Damit läuft alles mit
dynamischer Bindung: Lambdas schließen keine Variablen ein, der
Byte-Compiler warnt, und neuere Emacs-Versionen melden die fehlende Angabe
ausdrücklich. Zugleich fehlt je Datei die Beschreibung von Sinn und Zweck
(`;;; Commentary:`), die die Richtlinie für jeden Namespace verlangt.

**F2 – Drei Mechanismen für Einstellungen.** `setq`/`setq-default`,
`set-default` und `custom-set-variables` — Letzteres an sieben Stellen, auch
mitten in einem `use-package … :config`. `custom-set-variables` ist das
Ausgabeformat von Customize und gehört in `custom.el`; im Code ist es
irreführend, weil Customize beim Speichern dieselben Variablen in `custom.el`
schreibt und dann zwei Stände konkurrieren. Ersatz: `:custom` in
use-package bzw. `setopt` (Emacs 29) — beide rufen, anders als `setq`, die
Setter einer `defcustom` auf.

**F3 – Duplikate.** Der Block für strukturelles Editieren (≈ 25 Zeilen)
steht **siebenmal** im Code: in `clojure-mode-map`, zweimal hintereinander
in `cider-repl-mode-map`, in `emacs-lisp-mode-map`, `python-mode-map`,
`js2-mode-map` und `tide-mode-map`. Jede Abweichung zwischen den Kopien ist
heute Zufall oder Absicht, ohne dass man es sieht. Kleiner: Die
Host-Formatierung von `simple-httpd` steht in `base--development` und
`lang--html` je einmal.

**F4 – Formatierung.** Tabulatoren in zehn Dateien, 29 Zeilen über 80
Zeichen, ein deutscher Kommentar (`base--shell`). Alles drei widerspricht
der Richtlinie.

**F5 – Veraltetes und Totes.** Obsolet: `linum`, `x-select-enable-clipboard`,
`utf-translate-cjk-mode`, `(add-to-list 'image-types 'svg)` (Behelf für alte
macOS-Builds), `cider-grimoire` (aus CIDER entfernt), `scss-mode` (seit
Emacs 25 in `css-mode` enthalten). Tot: die `eval-sexp-fu-*`-Variablen (das
Paket wird nicht geladen), auskommentiertes PHP, `google-translate`,
`spacemacs-theme`.

**F6 – Hilfsfunktionen stecken in `:config`.** Fast 70 `defun`s sind in
use-package-Blöcken definiert, darunter 100 Zeilen Leiningen-/figwheel-Logik
in `cider`. Das funktioniert, aber der Byte-Compiler sieht sie erst zur
Laufzeit (Warnungen „function not known“), und Querbezüge sind unsichtbar:
`with-elscreen` (definiert bei `elscreen`) benutzt `ediff`,
`save-all-buffers` (bei `projectile`) hat mit Projekten nichts zu tun.

### use-package

**Ist use-package „the way to go“? Ja.** Es ist seit Emacs 29 eingebaut,
der faktische Standard, deklarativ und gut lesbar. Die Alternativen:

| Alternative | Was sie anders macht | Warum nicht hier |
|---|---|---|
| reines Elisp (`with-eval-after-load`, `keymap-set`, `add-hook`) | keine Makro-Magie, alles explizit | deutlich mehr Zeilen für dieselbe Aussage; das verzögerte Laden muss man selbst bauen |
| `leaf.el` | ähnlich wie use-package, strenger | Nischenlösung; kein Vorteil, der einen Wechsel trägt |
| `setup.el` | erweiterbar, minimalistisch | dasselbe; kleinere Gemeinschaft, weniger Beispiele |
| Literate Config (`config.org`, per org-babel „getangelt“) | Prosa und Code in einem Dokument | ein Übersetzungsschritt mehr; Fehlermeldungen zeigen auf die erzeugte Datei; Byte-Compiler und Linter sehen den Code nur mittelbar. Der Dokumentationsnutzen ist mit Dateiköpfen und Kommentaren billiger zu haben. |
| Starter-Kit (Doom, Spacemacs, Prelude) | fertige Gesamtkonfiguration | würde das eigene Tastaturkonzept überschreiben — das ist hier das Wertvollste. Einzelne Ideen lohnt es zu übernehmen, etwa die `early-init`-Voreinstellungen aus `minimal-emacs.d`. |

**Aber: use-package wird heute nicht idiomatisch benutzt.**

**U1 – `:bind*` als Standard.** Fast jede Belegung geht über `:bind*`,
also in die Override-Map, die **alle** Mode-Maps schlägt. Die README sagt
das Gegenteil („dont override global bindings“). Mit `:map` wirkt das
Sternchen gar nicht (siehe CLAUDE.md, WATCHOUT). Vorschlag: `:bind` als
Regel, `:bind*` nur für wenige, bewusst globale Tasten, mit Kommentar.

**U2 – `:requires` statt `:after`.** `:requires` überspringt die
Deklaration still, wenn das Feature beim Auswerten noch nicht geladen ist.
Bei `company-php :requires flycheck` wäre das der Fall gewesen (flycheck
lädt über `:hook` verzögert) — PHP ist auskommentiert, deshalb ist es
nicht aufgefallen.

**U3 – Widersprüche in einzelnen Blöcken.** `ztree` hat `:demand t` und
`:commands` (das eine lädt sofort, das andere verzögert); `elscreen` hat
zwei `:init`-Abschnitte; `aggressive-indent` schaltet den **globalen** Modus
aus dem Hook **jedes** Programmierpuffers ein; `pkg--*` haben ein
`:load-path`, obwohl `configuration/` schon im `load-path` steht.

**U4 – Bootstrap an use-package vorbei.** `diminish`, `smartrep`,
`doom-themes` und `exec-path-from-shell` werden von Hand installiert — und
zwar nur, wenn `doom-themes` fehlt. `exec-path-from-shell` wird danach nie
benutzt; der `PATH` wird stattdessen per `echo -n $PATH` übernommen, was in
einer GUI-Emacs unter macOS die Login-Shell nicht liest.

**Vorschlag für Konventionen** (kommen nach Absprache in die CLAUDE.md):

- Reihenfolge der Schlüsselwörter immer gleich: `:ensure`, `:demand`/
  `:defer`, `:after`, `:commands`, `:mode`, `:hook`, `:bind`, `:custom`,
  `:init`, `:config`. use-package ist das egal, dem Leser nicht.
- `:custom` für Variablen, `:config` nur für Code.
- Eingebaute Pakete mit `:ensure nil`, Emacs-Grundeinstellungen in einem
  `(use-package emacs …)`-Block statt verstreuter `setq`s.
- Kein `:requires`; `:after`, wo eine Reihenfolge nötig ist.
- Hilfsfunktionen ab einer Handvoll Zeilen in eine eigene Bibliothek unter
  `lisp/`, kleine als Top-Level-`defun` im Modul — nicht in `:config`.

### Paketverwaltung

**P1 – Archive über `http://`.** Pakete sind ausführbarer Code; sie
unverschlüsselt zu laden, ist eine offene Tür. Außerdem fehlt NonGNU ELPA,
und es gibt keine Prioritäten (welches Archiv gewinnt, wenn beide ein Paket
anbieten).

**P2 – Werkzeug.** Empfehlung: **package.el + use-package bleiben**,
ergänzt um `package-vc` für eigene Pakete direkt aus dem Git-Repo. Beides
ist eingebaut, ohne Fremdbootstrap. Der eine echte Nachteil: package.el
kennt keine Lockdatei; ein frischer Rechner bekommt die jeweils neueste
Version. Ist Reproduzierbarkeit über mehrere Rechner wichtig, ist
**elpaca** (oder `straight.el`) mit Lockdatei die Alternative — mit einem
Bootstrap-Block mehr und eigener Fehlersuche. Siehe Frage Q2.

---

## 3. Nebenbei gefundene Fehler

Aus dem Code geschlossen, nicht beobachtet. Behoben werden sie im
Paket-Review des jeweiligen Themas — oder vorgezogen, siehe Phase 1b.

| Nr. | Ort | Was passiert (vermutlich) |
|---|---|---|
| B1 | `base--organisation.el:125` | `("C--" . org-narrow-to-subtree)` steht vor `:map org-mode-map`, also **global** in der Override-Map. Weil Org nach `base--interface` lädt, verdrängt es dort `narrow/narrow-dwim`: `C--` lädt in jedem Puffer Org und ruft `org-narrow-to-subtree`. |
| B2 | `pkg--narrow.el:4` | `(narrow-to-region)` ohne Argumente — bei aktiver Region ein Fehler „wrong number of arguments“. |
| B3 | `lang--typescript.el:9` | `before-save`-Hook auf `tide-formater-before-save`; die Funktion heißt `tide-format-before-save`. Und der Hook ist global, nicht pufferlokal. Sobald einmal eine TS-Datei offen war, sollte **jedes** Speichern mit einem Fehler abbrechen. |
| B4 | `lang--javascript.el:83` | `"M-J "` (mit Leerzeichen) auf `xref-pop-maker-stack` — die Funktion heißt `xref-pop-marker-stack`, seit Emacs 29 `xref-go-back`. |
| B5 | `lang--typescript.el:57` | `"C-C M-j"` — großes C, also `C-S-c`; gemeint war wohl `C-c`. |
| B6 | `lang--basic.el:3` | `basic-mode` mit `:ensure nil`, ist aber ein MELPA-Paket — auf einem frischen Rechner fehlt es. Außerdem: `.inc`-Dateien als BASIC? |
| B7 | `base--development.el:153` | `prettify-symbols-alist` per `setq` statt `setq-local` — die Liste gilt für alle Puffer, auch für Modi mit eigener Liste. |

---

## 4. Zielbild

Die Projektstruktur der Richtlinie (`src/`, `resources/`, `target/`,
`dev/`, `scripts/`) passt nur sinngemäß: Emacs schreibt `init.el` und
`early-init.el` im Wurzelverzeichnis vor, und `src/jtkdvlp/<artifact>/`
wäre eine Ebene, die nichts trennt. Vorschlag — als bewusste Abweichung in
der CLAUDE.md festzuhalten:

```
~/.emacs.d/
├── early-init.el     vor dem ersten Frame: GC-Grenze, Paketsystem, Frame-Voreinstellungen
├── init.el           lädt die Module in fester Reihenfolge – sonst nichts
├── modules/          Konfiguration, ein Thema je Datei          (Rolle: src/)
│   ├── init-packages.el      Archive, Prioritäten, use-package-Voreinstellungen
│   ├── init-core.el          (use-package emacs …): Kodierung, Sicherungen, Grundverhalten
│   ├── init-keys.el          Tastaturkonzept: Präfix-Maps, repeat-mode, which-key
│   ├── init-completion.el    Vervollständigung im Minibuffer
│   ├── init-project.el
│   ├── init-windows.el       Fenster, Arbeitsbereiche, IDE-Layout
│   ├── init-editing.el
│   ├── init-structural-editing.el   die eine sexp-Belegung für alle Sprachen
│   ├── init-vc.el            magit, ediff
│   ├── init-shell.el
│   ├── init-org.el
│   ├── init-appearance.el
│   ├── init-lang-clojure.el, init-lang-elisp.el, …
│   └── init-logview.el, init-restclient.el, …   je Werkzeug, auch wenn klein
├── lisp/             eigene Bibliotheken, ohne Seiteneffekte    (Rolle: src/)
│   ├── jtk-ide.el, jtk-window-mirror.el, jtk-zoom.el, jtk-narrow.el
│   └── jtk-cider.el          Leiningen-/figwheel-Helfer aus lang--clojure
├── snippets/, eshell/        handgeschrieben, versioniert        (Rolle: resources/)
├── etc/              von Paketen geschriebene Konfiguration (no-littering),
│                     versioniert, was gepflegt ist (z. B. mc-lists)
├── var/              Zustand und Caches (no-littering), inkl. custom.el,
│                     nicht versioniert                            (Rolle: target/)
├── local.el          maschinenspezifisch (Jira-URL, …), nicht versioniert
├── elpa/, eln-cache/ nicht versioniert
└── dev/              plan.md, tmp/ (nicht versioniert)
```

Begründung der Ebenen nach der Frage „Was trennt sie *heute*?“:

- `modules/` ↔ `lisp/` trennt Konfiguration (Seiteneffekte beim Laden) von
  Bibliotheken (nur Definitionen). Bibliotheken lassen sich einzeln
  byte-kompilieren, testen und als Paket herauslösen.
- `etc/` ↔ `var/` übernimmt die Unterscheidung von
  [no-littering](https://github.com/emacscollective/no-littering): gepflegte
  Dateien gegen wegwerfbaren Zustand. `var/` ist damit fast das
  löschbare `target/` — bis auf Verläufe (recentf, savehist), die man beim
  Löschen verliert, ohne dass etwas kaputtgeht.
- Präfix `init-` für Module (verbreitet, z. B. bei purcell/emacs.d), `jtk-`
  für eigene Bibliotheken und deren Symbole — statt heute `zoom/inc`, was
  nicht der Elisp-Konvention entspricht und mit dem MELPA-Paket `zoom`
  kollidieren kann.

**Tastaturkonzept als eigenes Thema.** Die README beschreibt es, der Code
verteilt es. `init-keys.el` legt die Präfix-Maps an (`defvar-keymap` bzw.
`bind-keys :prefix-map`, mit which-key-Beschriftung), die Themen-Module
hängen sich hinein. Das strukturelle Editieren wird **eine** Keymap, die
jede Sprache übernimmt und nur dort überschreibt, wo sie wirklich abweicht
(JS: Zeilen statt sexps vertauschen; HTML: Elemente statt sexps).

**Sichtung billig machen.** Neben der Parallel-Instanz per
`--init-directory` (siehe CLAUDE.md) ein Prüfskript, das Emacs im
Batch-Modus mit der Konfiguration lädt und alle Module byte-kompiliert. Das
fängt Fehler wie B3 und B4 ohne Zutun — der Byte-Compiler meldet unbekannte
Funktionen. Optional dasselbe als GitHub-Action. Beides berührt `scripts/`
bzw. CI und braucht deine Freigabe (Q5).

---

## 5. Plan

Zwei Grundsätze für die Reihenfolge:

1. **Erst umziehen, dann umbauen.** Phase 1 ändert die Struktur, aber
   nicht das Verhalten. So bleibt jeder spätere Diff klein und zeigt nur
   die fachliche Änderung — und wenn nach Phase 1 etwas anders ist, ist es
   ein Fehler des Umzugs, keine Absicht.
2. **Im Paket-Review zuerst das, worauf anderes aufsetzt.** Wer zuerst
   eine Sprache aufräumt und danach die Vervollständigung tauscht, fasst
   die Sprache zweimal an.

### Phase 0 – Klären und messen (bei dir)

- Offene Fragen in Abschnitt 7 beantworten.
- Im Paketinventar (Abschnitt 6) die Spalte „benutzt?“ füllen. Was nicht
  mehr benutzt wird, fliegt ohne Review — das ist der billigste Gewinn.
- Baseline festhalten: Emacs-Version (`M-x emacs-version`), Startzeit
  (`M-x emacs-init-time`, dreimal, nach eingeschwungenem System), und mit
  `(setq use-package-compute-statistics t)` am Anfang von `init.el` die
  Ausgabe von `M-x use-package-report`. Die Zahlen kommen in die CLAUDE.md.

### Phase 1 – Fundament, verhaltensneutral (ein PR, mehrere Commits)

| Schritt | Inhalt | löst |
|---|---|---|
| 1.1 | `early-init.el`: GC-Grenze für den Start, `package-enable-at-startup`, Frame-Voreinstellungen (Leisten aus, maximiert) über `default-frame-alist` | S1 |
| 1.2 | Paketsystem: `https`, NonGNU ELPA, Prioritäten; Bootstrap-Pakete als normale use-package-Deklarationen; `exec-path-from-shell` statt PATH-Eigenbau | P1, U4 |
| 1.3 | `no-littering`; `.gitignore` auf `var/`, `elpa/`, `eln-cache/`, `local.el`, `dev/tmp/` reduzieren; `custom-file` nach `var/`, `custom.el` aus dem Repo; `local.el` für `env-jira-host-url` | S5, S6 |
| 1.4 | Dateiköpfe mit `lexical-binding` und Commentary; Tabs zu Leerzeichen; Kommentare englisch | F1, F4 |
| 1.5 | Umzug in `modules/` und `lisp/`: **nur `git mv` und Verschieben ganzer Blöcke**, kein Inhalt geändert, eigener Commit je Thema | S2–S4 |
| 1.6 | Einstellungen vereinheitlichen: `custom-set-variables` → `:custom`/`setopt`, Grundeinstellungen in `(use-package emacs …)` | F2 |
| 1.7 | Eigene Bibliotheken: Präfix `jtk-`, Hilfsfunktionen aus `:config` nach `lisp/` | F6 |
| 1.8 | Eine Keymap für strukturelles Editieren statt sieben Kopien; Abweichungen je Sprache sichtbar machen (vorher Tabelle der heutigen Unterschiede, damit nichts still verloren geht) | F3 |
| 1.9 | Prüfskript (Batch-Laden + Byte-Compile), falls freigegeben | — |

Toter Code und obsolete Aufrufe (F5) bleiben in Phase 1 stehen, wo ihr
Entfernen Verhalten ändert (`linum`), und fliegen, wo es keines ändert
(`eval-sexp-fu-*`, Auskommentiertes).

**Sichtung nach Phase 1:** Parallel-Instanz starten. Startet ohne Fehler
und ohne Warnungen im `*Messages*`-Puffer; Theme und Modeline wie bisher;
Stichproben der Tasten (`C-s`, `C-x f`, `M-#`, `M-v g`, `C-M-- t`,
`C-<rechts>` in Clojure und Elisp); ein Projekt öffnen, eine
Clojure-REPL starten, eine Org-Erfassung anlegen. Startzeit gegen die
Baseline.

### Phase 1b – bekannte Fehler (optional vorgezogen)

B1–B7 aus Abschnitt 3 sind jeweils wenige Zeilen. Entweder als kleiner PR
direkt nach Phase 1 — oder im Review des jeweiligen Themas. Deine Wahl.

### Phase 2 – Paket-Review

Je Block ein PR. Für jedes Paket dieselben Fragen, in dieser Reihenfolge:

1. Benutzt du es noch? — Nein: raus.
2. Kann Emacs das inzwischen selbst?
3. Wird es noch gepflegt?
4. Setzt es auf die Standard-Schnittstellen auf (siehe Abschnitt 1)?
5. Ist die Konfiguration minimal, und passen die Tasten ins Konzept?

| Block | Inhalt (heute) | Warum an dieser Stelle | Kandidaten |
|---|---|---|---|
| 2.1 Vervollständigung | ivy, counsel, swiper, counsel-projectile (counsel-Teil) | Am stärksten verflochten: Projectile, magit, eshell-Verlauf, Suche, Puffer- und Dateiwahl hängen daran. Jeder spätere Block müsste sonst zweimal angefasst werden. | vertico, orderless, marginalia, consult, embark — alle auf `completing-read` |
| 2.2 Tastatur-Infrastruktur | smartrep, which-key, diminish, `bind-key`-Gebrauch | Legt fest, wie Präfix-Maps und Wiederholungs-Tasten gebaut werden — das benutzen alle folgenden Blöcke. | `repeat-mode`, eingebautes which-key (30), `defvar-keymap`; Lichter in der Modeline ggf. über die Modeline-Wahl in 2.11 |
| 2.3 Projekte | projectile, treemacs-projectile | Eshell-Prompt, Fenstertitel, elscreen, httpd, CIDER-Helfer und dashboard fragen das Projekt ab. | `project.el` (eingebaut) — oder projectile bewusst behalten; beides vertretbar |
| 2.4 Fenster und Arbeitsbereiche | elscreen, winner, ace-window, buffer-move, fullframe, `pkg--ide`, `pkg--window-mirror`, ediff/ztree-Anbindung | Hängt an 2.3 (Arbeitsbereich je Projekt). elscreen ist seit Jahren ungepflegt. | `tab-bar-mode`, `windmove-swap-states-*`, Side-Windows in `display-buffer-alist` für das IDE-Layout |
| 2.5 Editieren | undo-tree, ace-jump-mode, expand-region, multiple-cursors, smartparens, aggressive-indent, highlight-symbol, kmacro, narrow | Grundlage für die Sprachen; die gemeinsame sexp-Keymap aus 1.8 hängt an smartparens. | vundo + `undo-redo`, avy, expreg, symbol-overlay; smartparens vs. paredit/puni |
| 2.6 Prüfen, Vervollständigen im Puffer, Snippets | company, company-quickhelp, flycheck, yasnippet, yasnippet-snippets | Vor den Sprachen, weil LSP (2.7) sich daran anschließt. | corfu + cape (auf `completion-at-point`), flymake (eingebaut, eglot nutzt es), yasnippet bleibt wohl |
| 2.7 Sprach-Infrastruktur | — (heute je Sprache eigene Lösung: tide, elpy, xref-js2, company-php) | Grundsatzentscheidung einmal treffen statt je Sprache. | eglot (eingebaut) + tree-sitter-Modi (`*-ts-mode`) |
| 2.8 Sprachen | nach Gewicht: Clojure (cider, clj-refactor, parseedn, flycheck-clj-kondo, Einrückungsregeln, Leiningen-Helfer), Elisp, JS/TS (js2-mode, js2-refactor, xref-js2, indium, tide, npm-mode), HTML/CSS (web-mode, web-mode-edit-element, scss-/sass-mode), Python (elpy), C++ (platformio-mode), PHP, BASIC | Setzt 2.5–2.7 voraus. Clojure zuerst, weil es am meisten benutzt wird. | Clojure: clojure-lsp über eglot **neben** CIDER, ggf. clojure-ts-mode, `deps.edn` neben Leiningen. Übrige: meist eglot + `*-ts-mode` |
| 2.9 Werkzeuge | magit (Gerrit), git-auto-commit-mode, ediff, ztree, eshell, eshell-up, esh-autosuggest, restclient, sql, simple-httpd, logview, csv-mode, markdown-mode, simple-bookmarks | Weitgehend unabhängig; nach den Grundlagen, weil Vervollständigung und Projekte hineinspielen. | magit bleibt; restclient ist meines Wissens archiviert → `verb` prüfen; eshell ggf. ergänzt um `eat` |
| 2.10 Organisation | org (GTD), dashboard | Unabhängig, aber persönlich — braucht dein Urteil mehr als Technik. | — |
| 2.11 Aussehen | doom-themes, spaceline, linum, rainbow-mode, rainbow-delimiters, Hervorhebung von `TODO`/`WATCHOUT`, Fenstertitel | Zuletzt: die Modeline zeigt Segmente aus 2.3, 2.4 und 2.6 (Projekt, IDE-Fenster, Prüfergebnisse) — erst wenn die feststehen, lohnt ihr Umbau. | doom-modeline oder eingebaute Modeline, `display-line-numbers-mode`, hl-todo |

### Phase 3 – Startzeit (nur bei Bedarf)

Gegen die Baseline aus Phase 0 messen. Erst wenn eine Zahl stört,
optimieren — `:demand t` reduzieren, `use-package-always-defer`,
native Kompilierung prüfen — und danach erneut messen.

---

## 6. Paketinventar

Spalte „benutzt?“ ist für dich. **e** = eingebaut, **M** = MELPA/ELPA.
„Einschätzung“ ist mein Stand ohne Netzrecherche; vor jeder Entscheidung
im Review wird der aktuelle Pflegestand nachgesehen.

| Paket | Quelle | heute in | wofür | Einschätzung | benutzt? |
|---|---|---|---|---|---|
| use-package | e | configuration | Deklaration | bleibt | |
| diminish | M | Bootstrap | Modeline-Lichter ausblenden | hängt an 2.11 | |
| smartrep | M | Bootstrap | Wiederholtasten | → `repeat-mode` | |
| exec-path-from-shell | M | Bootstrap | PATH aus Login-Shell | installiert, **nie benutzt** — einsetzen | |
| doom-themes | M | apperance | Theme `doom-one` | gepflegt | |
| spaceline | M | apperance | Modeline | kaum noch gepflegt | |
| linum | e | apperance | Zeilennummern | obsolet seit 29.1 | |
| rainbow-mode | M | apperance | Farbwerte einfärben | ok; in jedem `prog-mode` fraglich | |
| rainbow-delimiters | M | apperance | Klammern einfärben | ok | |
| kmacro | e | interface | Makros | ok | |
| ivy / counsel / swiper | M | interface | Vervollständigung, Suche | → 2.1 | |
| winner | e | interface | Fensterlayout zurück | ok, ggf. `tab-bar-history` | |
| fullframe | M | interface | magit im Vollbild | ggf. `display-buffer-alist` | |
| buffer-move | M | interface | Puffer tauschen | → `windmove-swap-states-*` | |
| elscreen | M | interface | Bildschirme je Projekt | ungepflegt → `tab-bar-mode` | |
| ediff | e | interface | Vergleich | ok | |
| ztree | M | interface | Verzeichnisvergleich | prüfen | |
| ace-jump-mode | M | interface | Springen | ungepflegt → avy | |
| ace-window | M | interface | Fensterwahl | gepflegt | |
| expand-region | M | interface | Auswahl erweitern | ok; expreg als Alternative | |
| which-key | M | interface | Tastenhilfe | eingebaut ab 30 | |
| undo-tree | M | interface | Undo-Baum | schwergewichtig → vundo | |
| multiple-cursors | M | interface | Mehrfach-Cursor | ok | |
| counsel-projectile / projectile | M | filemanagement | Projekte | → 2.1 / 2.3 | |
| git-auto-commit-mode | M | filemanagement | Auto-Commit (wofür?) | prüfen | |
| treemacs (+ -projectile, -icons-dired, -magit) | M | filemanagement | Dateibaum | gepflegt | |
| simple-bookmarks | M? | filemanagement | Lesezeichen, MySQL-Verbindungen | vermutlich eigenes Paket — bestätigen | |
| org | e | organisation | GTD | bleibt | |
| dashboard (+ page-break-lines) | M | startup | Startbildschirm | gepflegt | |
| smartparens | M | development | Klammern, sexp-Editieren | → 2.5 | |
| aggressive-indent | M | development | Einrücken beim Tippen | ok; Hook-Fehler U3 | |
| company / company-quickhelp | M | development | Vervollständigung im Puffer | → corfu? | |
| magit | M | development | Git | bleibt | |
| flycheck | M | development | Syntaxprüfung | → flymake? | |
| yasnippet / yasnippet-snippets | M | development | Snippets | ok | |
| highlight-symbol | M | development | Symbol hervorheben | ungepflegt → symbol-overlay | |
| restclient | M | development | HTTP-Anfragen | meines Wissens archiviert → verb | |
| sql | e | development | MySQL | ok | |
| simple-httpd | M | development | lokaler HTTP-Server | prüfen | |
| markdown-mode | M | development | Markdown | bleibt | |
| logview | M | development | Logdateien | gepflegt | |
| eshell / eshell-up / esh-autosuggest | e/M/M | shell | Shell | ok | |
| csv-mode | M | csv | CSV | ok | |
| clojure-mode | M | clojure | Clojure | ggf. clojure-ts-mode | |
| cider | M | clojure | REPL | bleibt | |
| clj-refactor | M | clojure | Refactoring | prüfen gegen clojure-lsp | |
| parseedn | M | clojure | `project.clj` lesen | hängt an Leiningen-Helfern | |
| flycheck-clj-kondo | M | clojure | Linter | hängt an 2.6 | |
| js2-mode / js2-refactor / xref-js2 | M | javascript | JS | → `js-ts-mode` + eglot | |
| indium | M | javascript | JS-Debugger | Pflegestand prüfen | |
| tide / npm-mode | M | typescript | TS | → `typescript-ts-mode` + eglot | |
| web-mode | M | html | HTML/XML/XSL | gepflegt | |
| web-mode-edit-element | M | html | strukturelles HTML-Editieren | vermutlich eigenes Paket — bestätigen | |
| scss-mode / sass-mode / css-mode | M/M/e | css | Stylesheets | scss-mode überflüssig | |
| elpy | M | python | Python | meines Wissens kaum gepflegt → eglot | |
| c++-mode / platformio-mode | e/M | c | Arduino/PlatformIO | prüfen | |
| basic-mode | M | basic | ASP/BASIC | Fehler B6 | |
| php-mode / geben / company-php | M | php (inaktiv) | PHP | auskommentiert — löschen? | |

---

## 7. Offene Fragen

- **Q1 – Zielversion und Plattformen.** Welche Emacs-Version läuft bei dir,
  und auf welchen Systemen (macOS, Linux)? Mein Vorschlag: Emacs 30 als
  Untergrenze — dann sind which-key, `use-package :vc` und die
  tree-sitter-Modi ohne Zusatz da.
- **Q2 – Reproduzierbarkeit.** Benutzt du die Konfiguration auf mehreren
  Rechnern, und stört es, wenn dort unterschiedliche Paketversionen
  landen? Nein → package.el bleibt (P2). Ja → elpaca mit Lockdatei.
- **Q3 – Zielbild.** Passt die Struktur aus Abschnitt 4 (`modules/`,
  `lisp/`, no-littering mit `etc/`/`var/`) und die Präfixe `init-` /
  `jtk-`?
- **Q4 – Eigene Pakete.** Sind `simple-bookmarks` und
  `web-mode-edit-element` deine? Dann würde ich sie per `package-vc`
  direkt aus deinem Repo beziehen, statt über MELPA.
- **Q5 – Prüfskript und CI.** Darf ein Skript unter `scripts/` (Batch-Laden
  und Byte-Compile) entstehen, und soll es zusätzlich als GitHub-Action
  laufen?
- **Q6 – Fehler B1–B7.** Vorziehen (Phase 1b) oder im jeweiligen Review?
