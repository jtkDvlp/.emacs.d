# Umbau der Emacs-Konfiguration – Befund und Plan

Stand: 2026-10-21. Grundlage ist der Commit `a116aa1` („Some settings“, März
2025). Alles hier ist **Lesart des Codes** — in der Umgebung, in der der
Plan entstand, läuft kein Emacs. Was als Fehler markiert ist, wurde nicht
beobachtet, sondern aus dem Code geschlossen.

**Fortschritt:** Phase 1, Schritte 1.0–1.3 sind umgesetzt (siehe dort);
der aktuelle Aufbau steht in der CLAUDE.md.

Inhalt:

1. Bestandsaufnahme
2. Befunde zur Grundstruktur (Dateiablage, Dateiaufbau, use-package,
   Paketverwaltung)
3. Nebenbei gefundene Fehler
4. Zielbild
5. Plan: Phasen und Reihenfolge des Paket-Reviews
6. Paketinventar
7. Entschiedene Fragen

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

**Entschieden (Q2): elpaca.** Gleiche Paketversionen sind wichtig. elpaca
klont jedes Paket als Git-Repo und hält die Commits in einer Lockdatei
fest; die kommt nach `etc/` und wird versioniert, der Klon-Baum
`elpaca/` nicht. Eigene Pakete bekommen ein Rezept auf ihr Repo. Was das
für Phase 1 heißt, steht dort unter 1.2.

---

## 3. Nebenbei gefundene Fehler

Aus dem Code geschlossen, nicht beobachtet. Behoben werden sie im
Paket-Review des jeweiligen Themas (entschieden, Q6) — manche Pakete
fallen dort ganz weg, und der Fehler mit ihnen.

| Nr. | Ort | Was passiert (vermutlich) |
|---|---|---|
| B1 | `base--organisation.el:125` | `("C--" . org-narrow-to-subtree)` steht vor `:map org-mode-map`, also **global** in der Override-Map. Weil Org nach `base--interface` lädt, verdrängt es dort `narrow/narrow-dwim`: `C--` lädt in jedem Puffer Org und ruft `org-narrow-to-subtree`. |
| B2 | `pkg--narrow.el:4` | `(narrow-to-region)` ohne Argumente — bei aktiver Region ein Fehler „wrong number of arguments“. Vom Byte-Compiler bestätigt. |
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
│                     (auch die handgeschriebenen yasnippet-Vorlagen und
│                     eshell-Aliase, Rolle: resources/)
├── etc/              von Paketen geschriebene Konfiguration (no-littering),
│                     versioniert, was gepflegt ist (z. B. mc-lists)
├── var/              Zustand und Caches (no-littering), inkl. custom.el,
│                     nicht versioniert                            (Rolle: target/)
├── local.el          maschinenspezifisch (Jira-URL, …), nicht versioniert
├── elpaca/, eln-cache/  Paket-Klone und native Kompilate, nicht versioniert
│                     (die elpaca-Lockdatei liegt versioniert in etc/)
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
Batch-Modus mit der Konfiguration lädt und alle Module byte-kompiliert.
Dasselbe läuft als GitHub-Action, damit jeder Push ohne lokales Emacs
geprüft wird. Beides ist freigegeben (Q5).

*Korrektur (nach Umsetzung):* Hier stand, das Prüfskript fange Fehler wie
B3 und B4 ohne Zutun. Das stimmt nicht. Funktionsnamen in `:hook`,
`:bind` und `:commands` sind für den Byte-Compiler Daten, kein Aufruf;
er meldet sie nicht. (B2 dagegen meldet er, weil dort eine Funktion
mit falscher Argumentzahl aufgerufen wird.) Das fängt erst eine Prüfung nach dem Laden, die
jedes gebundene Kommando und jede Hook-Funktion auf `fboundp` prüft — und
dafür müssten die verzögert geladenen Pakete erst geladen werden. Offener
Punkt für das Prüfskript.

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

- ~~Offene Fragen in Abschnitt 7 beantworten.~~ Erledigt, siehe dort.
- Die neueste stabile Emacs-Version installieren. Welche das ist und
  welcher macOS-Build sich empfiehlt (Kandidat: `emacs-plus` über Homebrew,
  mit nativer Kompilierung), sehe ich vor Phase 1 nach — siehe unten,
  „Netzzugriffe“.
- Im Paketinventar (Abschnitt 6) die Spalte „benutzt?“ füllen. Was nicht
  mehr benutzt wird, fliegt ohne Review — das ist der billigste Gewinn.
- Baseline festhalten: Emacs-Version (`M-x emacs-version`), Startzeit
  (`M-x emacs-init-time`, dreimal, nach eingeschwungenem System), und mit
  `(setq use-package-compute-statistics t)` am Anfang von `init.el` die
  Ausgabe von `M-x use-package-report`. Die Zahlen kommen in die CLAUDE.md.
  Gemessen wird mit der **heutigen Konfiguration auf dem neuen Emacs** —
  sonst vergleicht der Wert nach dem Umbau zwei Dinge auf einmal. Startet
  die heutige Konfiguration dort nicht, ist genau das der erste Befund.

### Phase 1 – Fundament, verhaltensneutral (ein PR, mehrere Commits)

**Netzzugriffe, am Stück vor Beginn:** aktuelle stabile Emacs-Version und
empfohlener macOS-Build; elpaca-Bootstrap-Block, Lockdatei-Funktionen und
use-package-Anbindung in der aktuellen Doku; Variablenliste von
no-littering (welche Pfade es umbiegt, etwa für eshell); die
GitHub-Action zum Installieren von Emacs (`purcell/setup-emacs`) und die
dort verfügbaren Versionen. Danach läuft Phase 1 ohne weitere Zugriffe —
bis auf die CI-Läufe selbst, die auf GitHub stattfinden.

| Schritt | Inhalt | löst |
|---|---|---|
| ✅ 1.0 | Prüfskript unter `scripts/` (Batch-Laden der Konfiguration, Byte-Compile aller Module, Warnungen gesammelt ausgeben) und GitHub-Action, die es bei jedem Push ausführt — **zuerst, gegen den heutigen Stand.** Die Warnungen von heute sind die Baseline; jeder folgende Schritt zeigt, ob er welche hinzufügt. | — |
| ✅ 1.1 | `early-init.el`: ~~GC-Grenze für den Start~~ (nicht ohne Messung, also Phase 3), `package-enable-at-startup nil` (verlangt elpaca ohnehin), Frame-Voreinstellungen (Leisten aus, maximiert) über `default-frame-alist` | S1 |
| ✅ 1.2 | Paketverwaltung auf elpaca: Bootstrap, `elpaca-use-package-mode`, Lockdatei nach `etc/`, Rezepte für die eigenen Pakete auf ihre Repos; Bootstrap-Pakete (`diminish`, `smartrep`, …) als normale Deklarationen; `exec-path-from-shell` statt PATH-Eigenbau | P1, P2, U4 |
| ✅ 1.3 | `no-littering`; `.gitignore` auf `var/`, `elpaca/`, `eln-cache/`, `local.el`, `dev/tmp/` reduzieren; `custom-file` nach `var/`, `custom.el` aus dem Repo; `local.el` für `env-jira-host-url` | S5, S6 |
| 1.4 | Dateiköpfe mit `lexical-binding` und Commentary; Tabs zu Leerzeichen; Kommentare englisch | F1, F4 |
| 1.5 | Umzug in `modules/` und `lisp/`: **nur `git mv` und Verschieben ganzer Blöcke**, kein Inhalt geändert, eigener Commit je Thema | S2–S4 |
| 1.6 | Einstellungen vereinheitlichen: `custom-set-variables` → `:custom`/`setopt`, Grundeinstellungen in `(use-package emacs …)` | F2 |
| 1.7 | Eigene Bibliotheken: Präfix `jtk-`, Hilfsfunktionen aus `:config` nach `lisp/` | F6 |
| 1.8 | Eine Keymap für strukturelles Editieren statt sieben Kopien; Abweichungen je Sprache sichtbar machen (vorher Tabelle der heutigen Unterschiede, damit nichts still verloren geht) | F3 |

**WATCHOUT für 1.2:** elpaca installiert und aktiviert Pakete
**asynchron, nach** dem Durchlauf von `init.el`. Code außerhalb eines
`use-package`-Blocks, der ein Paket voraussetzt — heute etwa
`(require 'smartrep)` und `(load-theme 'doom-one t)` auf oberster Ebene —
läuft dann, bevor das Paket da ist. Alles Paketabhängige gehört deshalb
in den jeweiligen Block (`:config`), notfalls hinter `elpaca-wait`. Und
eingebaute Pakete brauchen weiterhin `:ensure nil`, sonst holt elpaca eine
zweite Fassung aus Git (bei `org` womöglich gewollt — Entscheidung in 2.10).

Toter Code und obsolete Aufrufe (F5) bleiben in Phase 1 stehen, wo ihr
Entfernen Verhalten ändert (`linum`), und fliegen, wo es keines ändert
(`eval-sexp-fu-*`, Auskommentiertes).

**Ergebnis 1.0–1.3** (Messwerte in der CLAUDE.md):

- Die Baseline gibt es nur halb. Der Stand vor dem Umbau lädt im
  Batch-Modus nicht: `(scroll-bar-mode -1)` gibt es in einem Emacs ohne
  GUI nicht, und die Builds in CI haben keine. Die Zahl der Warnungen
  von heute ist damit nicht messbar; die erste Zahl ist die nach 1.3.
- Danach: Laden ok, 93 Pakete aus der Lockdatei, 0 Compile-Fehler, 52
  Warnungen. Davon 23 „file has no `lexical-binding' directive“ (eine
  je Konfigurationsdatei, das erledigt 1.4), 14 Funktionen aus Paketen,
  die erst zur Laufzeit geladen werden, und 15 Einzelbefunde — darunter
  B2 (`narrow-to-region` ohne Argumente) und das obsolete `linum`.
- Eine Neuinstallation aus der Lockdatei ergibt dieselben Commits; ein
  absichtlich älter gesetzter Commit wird genau so installiert; ein
  fehlender Eintrag lässt `--verify-lock` scheitern.

**Reihenfolge für 1.4–1.8:** 1.5 (Umzug) vor 1.4 (Dateiköpfe), weil der
Dateikopf den Dateinamen trägt — sonst würde jeder Kopf zweimal
geschrieben.

**Vorarbeit für 1.8 — die sieben Kopien heute.** „gleich“ heißt: derselbe
Befehl wie in Clojure; „fehlt“: die Taste ist in dieser Mode-Map nicht
belegt (es gilt dann, was global oder in der Mode selbst liegt).

| Taste | Befehl in Clojure | CIDER-REPL | Elisp | Python | JS (js2) | TS (tide) | HTML (web-mode) |
|---|---|---|---|---|---|---|---|
| `C-<left>` | `sp-backward-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-backward` |
| `M-<left>` | `sp-forward-barf-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-edit-element-elements-contract-over-border` |
| `C-M-<left>` | `sp-backward-barf-sexp` | gleich | gleich | gleich | gleich | gleich | fehlt |
| `C-<right>` | `sp-forward-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-forward` |
| `M-<right>` | `sp-forward-slurp-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-edit-element-elements-expand-over-border` |
| `C-M-<right>` | `sp-backward-slurp-sexp` | gleich | gleich | gleich | gleich | gleich | fehlt |
| `C-<up>` | `sp-backward-up-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-parent` |
| `M-<up>` | `sp-convolute-sexp` | gleich | gleich | gleich | fehlt | fehlt | `web-mode-element-vanish` |
| `C-<down>` | `sp-down-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-child` |
| `M-<down>` | `sp-raise-sexp` | gleich | gleich | gleich | `js2r-unwrap` | fehlt | `web-mode-edit-element-elements-raise` |
| `C-M-<up>` | `sp-backward-transpose-sexp` | gleich | gleich | gleich | `js2-mode-forward-transpose` | `js2-mode-forward-transpose` | `web-mode-edit-element-elements-transpose-backward` |
| `C-M-<down>` | `sp-forward-transpose-sexp` | gleich | gleich | gleich | `js2-mode-backward-transpose` | `js2-mode-backward-transpose` | `web-mode-element-transpose` |
| `M-"` | `sp-wrap-doublequote` | gleich | gleich | gleich | fehlt | fehlt | fehlt |
| `C-(` | `sp-wrap-round` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-wrap` |
| `M-[` | `sp-wrap-square` | gleich | gleich | gleich | gleich | gleich | fehlt |
| `M-{` | `sp-wrap-curly` | gleich | gleich | gleich | gleich | gleich | fehlt |
| `C-M-(` | `sp-rewrap-sexp` | gleich | gleich | gleich | fehlt | fehlt | `web-mode-element-rename` |
| `C-k` | `sp-kill-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-element-kill` |
| `C-S-K` | `sp-unwrap-sexp` | gleich | gleich | gleich | gleich | gleich | `web-mode-edit-element-elements-dissolve` |
| `M-k` | `sp-splice-sexp-killing-forward` | gleich | gleich | gleich | gleich | gleich | `web-mode-edit-element-elements-kill-siblings-next` |
| `M-K` | `sp-splice-sexp-killing-backward` | gleich | gleich | gleich | fehlt | fehlt | `web-mode-edit-element-elements-kill-siblings-previous` |
| `C-M-k` | `sp-splice-sexp-killing-around` | gleich | gleich | gleich | gleich | gleich | `web-mode-edit-element-elements-kill-siblings` |
| `C-;` | `comment-dwim` | `sp-comment-dwim` | gleich | gleich | fehlt | fehlt | `web-mode-comment-or-uncomment` |

Was die Tabelle zeigt:

- Clojure, CIDER-REPL, Elisp und Python sind gleich — bis auf `C-;` in der
  REPL (`sp-comment-dwim` statt `comment-dwim`). Das ist der gemeinsame
  Kern.
- JS und TS sind eine Teilmenge; vertauscht wird zeilenweise
  (`js2-mode-*-transpose`), und `M-<down>` ist in JS `js2r-unwrap`.
  Die TS-Map benutzt dabei Funktionen, die erst `js2-mode`s `:config`
  definiert — ohne geladenes `js2-mode` zeigen die Tasten ins Leere.
- HTML belegt dieselben Tasten mit Element-Befehlen und lässt einige
  frei. Das bleibt eine eigene Map; gemeinsam ist dort nur die
  Tastenbedeutung, nicht der Befehl.
- Ob die Lücken in JS/TS (`M-<up>`, `M-"`, `C-M-(`, `M-K`, `C-;`) Absicht
  sind oder Versehen, weißt nur du — **Frage an dich** vor 1.8.

**Umzug auf dem Mac**, einmal nach dem Ziehen dieses Stands. Vorher
sichern; Emacs dabei beenden:

```
cd ~/.emacs.d
cp -a . ~/emacs.d-backup-$(date +%F)

mkdir -p var/projectile var/treemacs var/eshell
[ -s custom.el ]               && mv custom.el var/custom.el
[ -f recentf ]                 && mv recentf var/recentf-save.el
[ -f bookmarks ]               && mv bookmarks var/bookmark-default.el
[ -f projectile-bookmarks.eld ] && mv projectile-bookmarks.eld var/projectile/known-projects.el
[ -f simple-bookmarks ]        && mv simple-bookmarks var/simple-bookmarks.el
[ -f .cache/treemacs-persist ] && mv .cache/treemacs-persist var/treemacs/persist.org
[ -f eshell/history ]          && mv eshell/history var/eshell/history
[ -d transient ]               && mv transient var/transient

# Nur noch Cache bzw. vom alten package.el; elpaca baut neu auf.
rm -rf elpa projectile.cache auto-save-list .cache
```

Was danach in `git status` als unversioniert auftaucht, stammt aus dem
alten Stand und gehört entweder nach `var/` oder weg. Der erste Start
klont alle Pakete (einige Minuten).

**Sichtung nach Phase 1:** Parallel-Instanz starten. Startet ohne Fehler
und ohne Warnungen im `*Messages*`-Puffer; Theme und Modeline wie bisher;
Stichproben der Tasten (`C-s`, `C-x f`, `M-#`, `M-v g`, `C-M-- t`,
`C-<rechts>` in Clojure und Elisp); ein Projekt öffnen, eine
Clojure-REPL starten, eine Org-Erfassung anlegen. Startzeit gegen die
Baseline.

Die Fehler B1–B7 aus Abschnitt 3 bleiben in Phase 1 bewusst stehen; sie
werden im Paket-Review behoben (Q6).

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
| use-package | e | configuration | Deklaration | bleibt | Ja |
| diminish | M | Bootstrap | Modeline-Lichter ausblenden | hängt an 2.11 | Ja |
| smartrep | M | Bootstrap | Wiederholtasten | → `repeat-mode` | Ja |
| exec-path-from-shell | M | Bootstrap | PATH aus Login-Shell | installiert, **nie benutzt** — einsetzen | |
| doom-themes | M | apperance | Theme `doom-one` | gepflegt | |
| spaceline | M | apperance | Modeline | kaum noch gepflegt | |
| linum | e | apperance | Zeilennummern | obsolet seit 29.1 | Ja |
| rainbow-mode | M | apperance | Farbwerte einfärben | ok; in jedem `prog-mode` fraglich | Ja |
| rainbow-delimiters | M | apperance | Klammern einfärben | ok | Ja |
| kmacro | e | interface | Makros | ok | |
| ivy / counsel / swiper | M | interface | Vervollständigung, Suche | → 2.1 | Ja |
| winner | e | interface | Fensterlayout zurück | ok, ggf. `tab-bar-history` | Ja |
| fullframe | M | interface | magit im Vollbild | ggf. `display-buffer-alist` | Ja |
| buffer-move | M | interface | Puffer tauschen | → `windmove-swap-states-*` | Ja |
| elscreen | M | interface | Bildschirme je Projekt | ungepflegt → `tab-bar-mode` | Ja |
| ediff | e | interface | Vergleich | ok | Ja |
| ztree | M | interface | Verzeichnisvergleich | prüfen | Ja |
| ace-jump-mode | M | interface | Springen | ungepflegt → avy | |
| ace-window | M | interface | Fensterwahl | gepflegt | Ja |
| expand-region | M | interface | Auswahl erweitern | ok; expreg als Alternative | Ja |
| which-key | M | interface | Tastenhilfe | eingebaut ab 30 | |
| undo-tree | M | interface | Undo-Baum | schwergewichtig → vundo | Ja |
| multiple-cursors | M | interface | Mehrfach-Cursor | ok | Ja |
| counsel-projectile / projectile | M | filemanagement | Projekte | → 2.1 / 2.3 | Ja |
| git-auto-commit-mode | M | filemanagement | Auto-Commit (wofür?) | prüfen | |
| treemacs (+ -projectile, -icons-dired, -magit) | M | filemanagement | Dateibaum | gepflegt | Ja |
| simple-bookmarks | M? | filemanagement | Lesezeichen, MySQL-Verbindungen | eigenes Paket (eigenes Repo), womöglich veraltet — Review wie ein Fremdpaket | Ja |
| org | e | organisation | GTD | bleibt | |
| dashboard (+ page-break-lines) | M | startup | Startbildschirm | gepflegt | |
| smartparens | M | development | Klammern, sexp-Editieren | → 2.5 | Ja |
| aggressive-indent | M | development | Einrücken beim Tippen | ok; Hook-Fehler U3 | Ja |
| company / company-quickhelp | M | development | Vervollständigung im Puffer | → corfu? | Ja |
| magit | M | development | Git | bleibt | Ja |
| flycheck | M | development | Syntaxprüfung | → flymake? | |
| yasnippet / yasnippet-snippets | M | development | Snippets | ok | Ja |
| highlight-symbol | M | development | Symbol hervorheben | ungepflegt → symbol-overlay | Ja |
| restclient | M | development | HTTP-Anfragen | meines Wissens archiviert → verb | Ja |
| sql | e | development | MySQL | ok | Ja |
| simple-httpd | M | development | lokaler HTTP-Server | prüfen | |
| markdown-mode | M | development | Markdown | bleibt | |
| logview | M | development | Logdateien | gepflegt | Ja |
| eshell / eshell-up / esh-autosuggest | e/M/M | shell | Shell | ok | Ja |
| csv-mode | M | csv | CSV | ok | |
| clojure-mode | M | clojure | Clojure | ggf. clojure-ts-mode | Ja |
| cider | M | clojure | REPL | bleibt | Ja |
| clj-refactor | M | clojure | Refactoring | prüfen gegen clojure-lsp | Ja |
| parseedn | M | clojure | `project.clj` lesen | hängt an Leiningen-Helfern | |
| flycheck-clj-kondo | M | clojure | Linter | hängt an 2.6 | |
| js2-mode / js2-refactor / xref-js2 | M | javascript | JS | → `js-ts-mode` + eglot | |
| indium | M | javascript | JS-Debugger | Pflegestand prüfen | |
| tide / npm-mode | M | typescript | TS | → `typescript-ts-mode` + eglot | |
| web-mode | M | html | HTML/XML/XSL | gepflegt | |
| web-mode-edit-element | M | html | strukturelles HTML-Editieren | eigenes Paket (eigenes Repo), womöglich veraltet — Review wie ein Fremdpaket | |
| scss-mode / sass-mode / css-mode | M/M/e | css | Stylesheets | scss-mode überflüssig | Ja |
| elpy | M | python | Python | meines Wissens kaum gepflegt → eglot | |
| c++-mode / platformio-mode | e/M | c | Arduino/PlatformIO | prüfen | |
| basic-mode | M | basic | ASP/BASIC | Fehler B6 | |
| php-mode / geben / company-php | M | php (inaktiv) | PHP | auskommentiert — löschen? | |

---

## 7. Entschiedene Fragen

Am 2026-10-21 beantwortet; die Begründungen stehen in der CLAUDE.md unter
„Entscheidungen“.

| Frage | Antwort |
|---|---|
| Q1 – Zielversion und Plattformen | Neueste stabile Emacs-Version, nur macOS. |
| Q2 – Reproduzierbarkeit über Rechner hinweg | Wichtig → elpaca mit Lockdatei statt package.el. |
| Q3 – Zielbild (`modules/`, `lisp/`, `etc/`/`var/`, Präfixe `init-`/`jtk-`) | Passt. |
| Q4 – `simple-bookmarks`, `web-mode-edit-element` | Eigene Pakete mit eigenem Repo, womöglich veraltet → Bezug aus dem Repo, Review wie Fremdpakete. |
| Q5 – Prüfskript unter `scripts/` und GitHub-Action | Beides freigegeben → Phase 1, Schritt 1.0. |
| Q6 – Fehler B1–B7 vorziehen? | Nein, im Review des jeweiligen Pakets. |
