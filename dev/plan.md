# Umbau der Emacs-Konfiguration – Befund und Plan

Stand: 2026-10-21. Grundlage ist der Commit `a116aa1` („Some settings“, März
2025). Alles hier ist **Lesart des Codes** — in der Umgebung, in der der
Plan entstand, läuft kein Emacs. Was als Fehler markiert ist, wurde nicht
beobachtet, sondern aus dem Code geschlossen.

**Fortschritt:** Phase 1 ist umgesetzt (siehe dort, mit Ergebnis);
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
| `base--development.el` | 344 | Klammern, Einrückung, Vervollständigung im Buffer, Git, Syntaxprüfung, Snippets, Symbol-Hervorhebung, REST-Client, SQL, HTTP-Server, Markdown, Log-Betrachter, IDE-Layout |
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
aus dem Hook **jedes** Programmier-Buffers ein; `pkg--*` haben ein
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
| B1 ✅ (2.2) | `base--organisation.el:125` | `("C--" . org-narrow-to-subtree)` steht vor `:map org-mode-map`, also **global** in der Override-Map. Weil Org nach `base--interface` lädt, verdrängt es dort `narrow/narrow-dwim`: `C--` lädt in jedem Buffer Org und ruft `org-narrow-to-subtree`. |
| B2 ✅ (2.2) | `pkg--narrow.el:4` | `(narrow-to-region)` ohne Argumente — bei aktiver Region ein Fehler „wrong number of arguments“. Vom Byte-Compiler bestätigt. |
| B3 | `lang--typescript.el:9` | `before-save`-Hook auf `tide-formater-before-save`; die Funktion heißt `tide-format-before-save`. Und der Hook ist global, nicht buffer-lokal. Sobald einmal eine TS-Datei offen war, sollte **jedes** Speichern mit einem Fehler abbrechen. |
| B4 ✅ (2.2) | `lang--javascript.el:83` | `"M-J "` (mit Leerzeichen) auf `xref-pop-maker-stack` — die Funktion heißt `xref-pop-marker-stack`, seit Emacs 29 `xref-go-back`. |
| B5 ✅ (2.2) | `lang--typescript.el:57` | `"C-C M-j"` — großes C, also `C-S-c`; gemeint war wohl `C-c`. |
| B6 | `lang--basic.el:3` | `basic-mode` mit `:ensure nil`, ist aber ein MELPA-Paket — auf einem frischen Rechner fehlt es. Außerdem: `.inc`-Dateien als BASIC? |
| B7 | `base--development.el:153` | `prettify-symbols-alist` per `setq` statt `setq-local` — die Liste gilt für alle Buffer, auch für Modi mit eigener Liste. |

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
| ✅ 1.4 | Dateiköpfe mit `lexical-binding` und Commentary; Tabs zu Leerzeichen; Kommentare englisch. ~~Zeilen über 80 Zeichen~~ → Phase 2, siehe unten | F1, F4 |
| ✅ 1.5 | Umzug in `modules/` und `lisp/`: **nur `git mv` und Verschieben ganzer Blöcke**, kein Inhalt geändert, eigener Commit je Thema | S2–S4 |
| ✅ 1.6 | Einstellungen vereinheitlichen: `custom-set-variables` → `:custom`/`setopt`. ~~`(use-package emacs …)`~~ — `setopt` in `init-core` sagt dasselbe ohne Makro | F2 |
| ✅ 1.7 | Eigene Bibliotheken: Präfix `jtk-`. ~~Hilfsfunktionen aus `:config` nach `lisp/`~~ → Phase 2, siehe unten | F6 |
| ✅ 1.8 | Eine Keymap für strukturelles Editieren statt sieben Kopien; Abweichungen je Sprache sichtbar machen (vorher Tabelle der heutigen Unterschiede, damit nichts still verloren geht) | F3 |

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

**Ergebnis 1.4–1.8:**

- Jeder Schritt wurde gegen den Schnappschuss geprüft (Tastenbelegungen,
  Optionen, Hooks, Dateizuordnungen nach dem Laden aller Pakete). Leer
  blieb der Diff bei: Umzug der Sprachmodule, Dateiköpfen,
  `lexical-binding`, Tabs, Übersetzungen, toter Konfiguration. Beim
  Aufteilen der `base--`-Module änderte sich nur die Reihenfolge in
  `prog-mode-hook`; die dadurch getauschten Keymaps teilen keine Taste.
  Bei `setopt` fiel `inhibit-startup-echo-area-message` weg (war `t` und
  damit nie wirksam). Die `jtk-`-Umbenennung zeigt genau die neuen Namen,
  die gemeinsame Struktur-Keymap genau die fünf neuen Tasten in
  JavaScript.
- Beim Aufteilen sind alle 107 Formen wortgleich wieder angekommen
  (eigener Vergleich, Kommentare ausgenommen).
- `lexical-binding` hätte eine Stelle still gebrochen: `sql-mysql-for`
  (MySQL-Lesezeichen) bindet Variablen von `sql.el`, bevor es geladen
  ist. Behoben; alle anderen `let`-Bindungen auf Paket-Variablen geprüft.
- Warnungen: 31 statt 52 (nach 1.3).

**Bewusst nach Phase 2 verschoben**, weil es die Stellen ohnehin anfasst
und sonst zweimal: die 34 Zeilen über 80 Zeichen (ohne den elpaca-
Installer, der wörtlich wie upstream bleibt), die Hilfsfunktionen in
`:config` (F6), `:bind*` ohne Absicht (U1), `:requires` (U2), die
Widersprüche in einzelnen Blöcken (U3).

**Neue Befunde für Phase 2** (aus Schnappschuss und Prüfskript):

- `which-key` entfernt (2026-10-25): Das GNU-ELPA-Rezept klonte das ganze
  Emacs-Repository, auf dem Mac hing daran der erste Start. Das
  Prüfskript scheitert seitdem an solchen Paketen.
- `typescript-mode` ist nicht installiert — die aktuelle `tide`-Fassung
  zieht es nicht mehr mit. Der ganze `tide`-Block (`:after
  typescript-mode`) läuft deshalb nie; die TypeScript-Konfiguration ist
  wirkungslos, B3 damit auch.
- `basic-mode` ist nicht installiert (B6 bestätigt).
- `scss-mode` ist installiert, lässt sich aber nicht laden.
- `indium` legt `indium-chrome-profile/` im Wurzelverzeichnis an.
- `success-face` ist ohne `eval-sexp-fu` unbenutzt.

**Sichtung nach Phase 1:** Parallel-Instanz starten. Startet ohne Fehler
und ohne Warnungen im `*Messages*`-Buffer; Theme und Modeline wie bisher;
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
| 2.1 Vervollständigung | ivy, counsel, swiper, counsel-projectile (counsel-Teil) | Am stärksten verflochten: Projectile, magit, eshell-Verlauf, Suche, Buffer- und Dateiwahl hängen daran. Jeder spätere Block müsste sonst zweimal angefasst werden. | vertico, orderless, marginalia, consult, embark — alle auf `completing-read` |
| 2.2 Tastatur-Infrastruktur | smartrep, which-key, diminish, `bind-key`-Gebrauch | Legt fest, wie Präfix-Maps und Wiederholungs-Tasten gebaut werden — das benutzen alle folgenden Blöcke. | `repeat-mode`, eingebautes which-key (30), `defvar-keymap`; Lichter in der Modeline ggf. über die Modeline-Wahl in 2.11 |
| 2.3 Projekte | projectile, treemacs-projectile | Eshell-Prompt, Fenstertitel, elscreen, httpd, CIDER-Helfer und dashboard fragen das Projekt ab. | `project.el` (eingebaut) — oder projectile bewusst behalten; beides vertretbar |
| 2.4 Fenster und Arbeitsbereiche | elscreen, winner, ace-window, buffer-move, fullframe, `pkg--ide`, `pkg--window-mirror`, ediff/ztree-Anbindung | Hängt an 2.3 (Arbeitsbereich je Projekt). elscreen ist seit Jahren ungepflegt. | `tab-bar-mode`, `windmove-swap-states-*`, Side-Windows in `display-buffer-alist` für das IDE-Layout |
| 2.5 Editieren | undo-tree, ~~ace-jump-mode, expand-region~~ (in 2.2 ersetzt), multiple-cursors, smartparens, aggressive-indent, highlight-symbol, kmacro, narrow | Grundlage für die Sprachen; die gemeinsame sexp-Keymap aus 1.8 hängt an smartparens. | vundo + `undo-redo`, avy, expreg, symbol-overlay; smartparens vs. paredit/puni |
| 2.6 Prüfen, Vervollständigen im Buffer, Snippets | company, company-quickhelp, flycheck, yasnippet, yasnippet-snippets | Vor den Sprachen, weil LSP (2.7) sich daran anschließt. | corfu + cape (auf `completion-at-point`), flymake (eingebaut, eglot nutzt es), yasnippet bleibt wohl |
| 2.7 Sprach-Infrastruktur (LSP) | — (heute je Sprache eigene Lösung: tide, elpy, xref-js2, company-php) | Grundsatzentscheidung einmal treffen statt je Sprache. Nach 2.1 und 2.6, weil ein LSP-Client Vervollständigung und Prüfung beliefert. | **Neu aufgenommen (2026-10-25):** LSP-Client plus Language-Server je Sprache, dazu tree-sitter-Modi (`*-ts-mode`); Kandidaten im Inventar unter „Neue Pakete“ |
| 2.8 Sprachen | nach Gewicht: Clojure (cider, clj-refactor, parseedn, flycheck-clj-kondo, Einrückungsregeln, Leiningen-Helfer), Elisp, JS/TS (js2-mode, js2-refactor, xref-js2, indium, tide, npm-mode), HTML/CSS (web-mode, web-mode-edit-element, scss-/sass-mode), Python (elpy), C++ (platformio-mode), PHP, BASIC | Setzt 2.5–2.7 voraus. Clojure zuerst, weil es am meisten benutzt wird. | Clojure: clojure-lsp über eglot **neben** CIDER, ggf. clojure-ts-mode, `deps.edn` neben Leiningen. Übrige: meist eglot + `*-ts-mode` |
| 2.9 Werkzeuge | magit (Gerrit), git-auto-commit-mode, ediff, ztree, eshell, eshell-up, esh-autosuggest, restclient, sql, simple-httpd, logview, csv-mode, markdown-mode, simple-bookmarks | Weitgehend unabhängig; nach den Grundlagen, weil Vervollständigung und Projekte hineinspielen. | magit bleibt; restclient ist meines Wissens archiviert → `verb` prüfen; eshell ggf. ergänzt um `eat` |
| 2.10 Organisation | org (GTD), dashboard | Unabhängig, aber persönlich — braucht dein Urteil mehr als Technik. | — |
| 2.11 Aussehen | doom-themes, spaceline, linum, rainbow-mode, rainbow-delimiters, Hervorhebung von `TODO`/`WATCHOUT`, Fenstertitel | Zuletzt: die Modeline zeigt Segmente aus 2.3, 2.4 und 2.6 (Projekt, IDE-Fenster, Prüfergebnisse) — erst wenn die feststehen, lohnt ihr Umbau. | doom-modeline oder eingebaute Modeline, `display-line-numbers-mode`, hl-todo |

#### ✅ 2.1 Vervollständigung (entschieden und umgesetzt 2026-10-25)

**Ersatz:** ivy, counsel, swiper und counsel-projectile durch
**vertico** (Liste im Minibuffer), **orderless** (Leerzeichen trennt
Suchbegriffe in beliebiger Reihenfolge, wie bei ivy), **marginalia**
(Zusatzinfos neben den Einträgen, etwa Tastenbelegung bei `M-x`),
**consult** (Befehle mit Vorschau: Buffer, Zeilen, grep, Verlauf) und
**embark** mit **embark-consult** (Aktionen auf einem Eintrag, Export in
einen Buffer). Alle setzen auf `completing-read` auf: Jeder Befehl, der
eine Auswahl fragt — auch magit, projectile und die CIDER-Helfer —,
bekommt dieselbe Oberfläche ohne eigenen Klebstoff. Verworfen:
ivy behalten (eigene Welt, siehe Abschnitt 1); nur eingebautes
`fido-vertical-mode` (keine Vorschau, kein Gegenstück zu `swiper`).

**Tasten bleiben, wo sie sind.** Was sich dahinter ändert:

| Taste | heute | künftig | Unterschied für dich |
|---|---|---|---|
| (alle Auswahlen) | `ivy-mode` | `vertico-mode` + orderless + marginalia | gleiche Bedienung; 15 Zeilen, Umlauf am Ende wie bisher |
| `M-x`, `C-h f`, `C-h v` … | von `counsel-mode` umgelenkt | die eingebauten Befehle in vertico | Tastenbelegung und Doku-Zeile stehen per marginalia daneben |
| `C-o` in einer Auswahl | `ivy-occur` | `embark-export` | dasselbe: Liste in einen Buffer |
| `C-s`, `C-r` | `swiper` | `consult-line` | mit Vorschau beim Blättern |
| `C-r` in der Suche | `swiper-query-replace` | — | **entfällt:** stattdessen `C-o` (Export nach occur), dort `e` zum Bearbeiten |
| `C-S-s` | `counsel-git-grep` (Region als Vorgabe) | `consult-git-grep` (Region als Vorgabe) | mit Vorschau |
| `C-S-r` | `counsel-git-grep-query-replace` | `consult-git-grep`, `C-o`, dann bearbeiten (Paket **wgrep**) | Ersetzen über mehrere Dateien in einem bearbeitbaren grep-Buffer statt per Abfrage |
| `C-S-y` | `counsel-yank-pop` | `consult-yank-pop` | mit Vorschau |
| `C-x b` | `counsel-switch-buffer` | `consult-buffer` | zeigt auch zuletzt geöffnete Dateien (wie `ivy-use-virtual-buffers`) |
| `C-x B` | `counsel-projectile-switch-to-buffer` | `consult-project-buffer` | Buffer und zuletzt benutzte Dateien des Projekts |
| `C-x f` | `counsel-find-file` | `find-file` | — |
| `C-x F` | `counsel-projectile-find-file` | `projectile-find-file` | — |
| `C-x C-f` | `counsel-recentf` | `consult-recent-file` | mit Vorschau |
| `C-x D` | `counsel-projectile-find-dir` | `projectile-find-dir` | — |
| `C-x x f` | `counsel-projectile-switch-project` | `projectile-switch-project` | — |
| `C-x x F` | `counsel-projectile` (Buffer und Dateien in einer Liste) | `consult-project-buffer` | wie `C-x B` |
| `M-P` in eshell | `counsel-esh-history` | `consult-history` | — |

**Nebenbei:** `delete-selection-mode` stand in counsels `:config` und
zieht nach `init-editing`; `magit` und `projectile` verlieren ihre
ivy-Einstellungen (beide nehmen dann einfach `completing-read`);
`etc/mc-list.el` nennt `counsel-M-x` und `counsel-yank-pop`, die durch ihre
Nachfolger ersetzt werden.

**Entschieden:** vertico und Verwandte ja; Ersetzen über Export und
Bearbeiten im Buffer; embark ohne eigene Taste, nur `C-o`.

**Abweichung beim Umsetzen:** wgrep war freigegeben, wird aber nicht
gebraucht. Emacs 31 macht grep-Buffer selbst bearbeitbar
(`grep-edit-mode`: `e`, dann `C-c C-c`); geprüft an einem Buffer in der
Form, die embark-consult beim Export erzeugt. Die Änderungen landen in
den Buffern der Dateien und werden wie gewohnt gespeichert.

**Ergänzt, weil der Schnappschuss es zeigte:** counsel belegte im
Minibuffer `C-r` mit der Suche im Verlauf; das macht jetzt
`consult-history`. Dazu kamen `savehist-mode` (vertico sortiert nach dem
Verlauf, der jetzt Neustarts überlebt) und `recentf-mode` ab dem Start
(`consult-recent-file` schaltet es, anders als `counsel-recentf`, nicht
selbst ein).

#### ✅ 2.2 Tastatur-Infrastruktur (entschieden und umgesetzt 2026-10-28)

**Ersatz:** smartrep durch das eingebaute **`repeat-mode`**. Die vier
Wiederhol-Präfixe sind `defvar-keymap`s mit `:repeat`:

| Präfix | Map | Wiederholbare Tasten |
|---|---|---|
| `M-+ f` | `jtk-zoom-map` | `# + - 1 2 3` (Schriftgröße) |
| `M-+ w` | `jtk-frame-map` | `#` (Frame maximieren) |
| `M-m` | `jtk-highlight-symbol-map` | `n p r` (Symbol vor, zurück, ersetzen) |
| `C-a` in HTML | `jtk-html-attribute-map` | `C-<left>`, `C-<right>`, `C-M-<left>`, `C-M-<right>`, `C-(`, `C-M-(`, `C-k`, `M-SPC` (Attribute) |

Mit `repeat-mode` gelten auch die Wiederhol-Maps, die Emacs mitbringt
(`C-x o o`, `C-x u u`, `M-g n n` …). `repeat-check-key` steht auf dem
Standard `t`: Eine Map greift nur, wenn die letzte Taste des Aufrufs in
ihr steht — `M-x highlight-symbol-next` startet also keine Folge.

**Entschieden und umgesetzt:**

| Nr. | Was | Ergebnis |
|---|---|---|
| A | Wiederholtasten | `repeat-mode` statt smartrep, mit Emacs' eigenen Maps; smartrep aus der Lockdatei |
| B | `:bind*` | `:bind` als Regel. Nur `C-#` (Org-Tabellen) und `C-:` (clojure-mode) bleiben in der Override-Map |
| C | Fehler B1, B2 | Org legt `C--` in `org-mode-map`; `jtk-narrow-dwim` verengt auf die Region |
| D | `("C-v" . nil)` | gestrichen, wirkte nie |
| E | Tastenhilfe | `C-h` nach einem Präfix ruft `embark-prefix-help-command` |
| F | Verlaufssuche im Minibuffer | `M-P` (wie eshell und CIDER-REPL); `C-r` tut dort nichts (`ignore`), statt auf das globale `consult-line` durchzufallen |
| G | Präfixe | benannte Präfix-Map je Modul, Tabelle in der README |
| H | Kleinkram | `C-m`/`RET`-Trennung nach `init-keys`; macOS-Modifier ohne `when` |

`diminish` bleibt bis 2.11.

**Was der Vergleich der wirksamen Tasten zeigte:** Verglichen wurde je
Buffer-Typ (Grundmodus, Elisp, Org, Clojure, HTML mit
`web-mode-edit-element`, Logview, Minibuffer, vertico), was
`key-binding` für jede belegte Taste liefert, vorher gegen nachher. Außer
den Präfixen, die jetzt einen Namen statt einer namenlosen Keymap
liefern, änderte sich nur:

| Taste | Buffer | vorher | nachher |
|---|---|---|---|
| `C--` | alle außer Org | `org-narrow-to-subtree` (B1) | `jtk-narrow-dwim` |
| `C-- s`, `C-- t`, `C-+` | Logview | von der Override-Map verdeckt | logviews eigene Befehle |
| `C-r` | Minibuffer | `consult-history` | nichts (`ignore`) |
| `M-P` | Minibuffer | — | `consult-history` |
| `M-'` | Minibuffer | `M-' c`/`C` aus der Override-Map | `undefined`, wie elscreen es dort setzt |

**Nachgezogen auf Wunsch (Vorschläge 1 und 4 aus dem Bericht zu 2.2):**

- **Sprungtasten für alle Sprachen gleich.** `M-j d` (Definition),
  `M-j r` (Verwendungen), `M-J` (zurück) und `C-h h` (Doku an der
  Stelle) sind global und gehen über xref und eldoc. Eine Sprache
  belegt eine Taste nur noch, wo sie Besseres hat: CIDER, elpy und
  tide für die Definition, CIDER und tide für die Doku; CIDERs `M-j r`
  bleibt `cider-apropos`. Gestrichen sind die Belegungen, die nur das
  Globale wiederholten, darunter B4; B5 ist korrigiert. Mit eglot (2.7)
  füllt dann der Language-Server dieselben Tasten.
- **xref-js2 hing global am Hook** und hätte nach dem ersten JS-Buffer
  `M-j d` in jedem Modus ohne eigenes Backend übernommen; jetzt nur in
  js2-mode-Buffern.
- **avy statt ace-jump-mode, expreg statt expand-region**, aus 2.5
  vorgezogen, gleiche Tasten. Die beiden HTML-Befehle, die
  expand-region-Funktionen riefen, markieren selbst; verglichen in einem
  web-mode-Buffer: gleiche Regionen, und ein Wert in einfachen
  Anführungszeichen wird jetzt markiert, wo expand-region nichts tat.

**Nebenbei aufgefallen:** Die elscreen-Tasten unter `M-'` außer `c`
und `C` wirkten erst, nachdem elscreen geladen war; davor kannte `M-'`
nur die beiden Override-Tasten. Jetzt lädt `M-'` elscreen beim ersten
Druck (`:bind-keymap`), und alle Tasten wirken sofort.

#### ✅ 2.3 Projekte (entschieden und umgesetzt 2026-10-28)

**Ersatz:** projectile durch das eingebaute **project.el**. consult,
xref, magit und ab 2.7 eglot fragen ohnehin project.el; projectile-mode
hängte sich bisher selbst dort ein (`project-find-functions`), sodass
alle über projectile liefen. Verworfen: projectile behalten — dann gäbe
es ab 2.7 zwei Projektbegriffe.

| Taste | vorher | nachher |
|---|---|---|
| `C-x F` | `projectile-find-file` | `project-find-file` |
| `C-x d` / `C-x D` | `projectile-dired` / `projectile-find-dir` | `project-dired` / `project-find-dir` |
| `C-x K` | `projectile-kill-buffers` | `project-kill-buffers` |
| `C-x x f` | `projectile-switch-project` (gleich die Dateiauswahl) | `project-switch-project`: RET öffnet die Dateiauswahl, `M-x` bietet die Projektbefehle an (2.3a) |
| `C-x C-S-F` | `projectile-recentf` | `consult-project-buffer` (wie `C-x B`; die Liste enthält die zuletzt benutzten Dateien des Projekts) |
| `M-! s s` | `projectile-run-eshell` | `project-eshell` |
| `M-' c` | `projectile-elscreen-create` | `jtk-elscreen-create-for-project` (umbenannt) |
| `M-x` | `projectile-httpd/start`, `…/stop` | `jtk-httpd-start-project`, `jtk-httpd-stop-project` (umbenannt) |

Was nach dem Projektwechsel geschieht und wo die Projektbefehle liegen,
regelt 2.3a. Ein Zwischenstand dieses PRs bot nach jedem Wechsel eine
Liste an; 2.3a hat ihn abgelöst.

**Umgesetzt:** Wurzel und Name des Projekts kommen aus `lisp/jtk-project.el`
(`jtk-project-root`, `jtk-project-name`) — für Fenstertitel,
eshell-Prompt, elscreen, httpd, CIDER-Helfer und HTML-Vorschau. Die
ignorierten Ordner stehen in `project-vc-ignores`; `.projectile` bleibt
eine Projektwurzel. Das IDE-Layout kennt den eshell-Buffernamen von
project.el. consults projectile-Adapter und treemacs-projectile fallen
weg.

**Geprüft** in einem Test-Repository: Wurzel absolut, Name, Fenstertitel
und eshell-Prompt wie zuvor; `node_modules/` und `dist/` fehlen in der
Dateiliste, ein Unterordner mit `.projectile` ist ein eigenes Projekt;
`project-eshell` heißt `*repo-eshell*` und landet im IDE-Fenster.
`project-current` kostet 0,015 ms je Aufruf (Emacs 31.1, 1000 Aufrufe),
der Fenstertitel braucht also keinen Zwischenspeicher.

**Unterschied, der bleibt:** Ein Ordner **ohne git** mit `project.clj`,
`deps.edn` oder `package.json` war für projectile ein Projekt, für
project.el ist er keins. Abhilfe bei Bedarf: diese Dateien in
`project-vc-extra-root-markers` aufnehmen — dann wird allerdings auch
jedes Unterprojekt in einem git-Repository zu einem eigenen Projekt.

**Nebenbei:** Das Dashboard holte seine Projektliste schon immer aus
project.el (`dashboard-projects-backend` ist `project-el`), nicht aus
projectile; die Liste füllt sich jetzt mit den benutzten Projekten.

#### ✅ 2.3a Standard, Ausnahme, Suche (entschieden und umgesetzt 2026-10-29)

Anlass: Das Projektmenü aus 2.3 (`jtk-project-act`, eine Liste nach jedem
Projektwechsel) funktioniert, kostet aber im Normalfall einen Schritt, und
Kürzel fehlen. Daraus ist eine allgemeinere Frage geworden: wie Auswahl,
Menü und Befehlssuche zusammenpassen, ohne viele Mechanismen.

**Anforderungen**

1. Projektwechsel: Im Normalfall eine Datei im Projekt öffnen; als
   Ausnahme dort eshell, Magit oder anderes.
2. Die Ausnahmen erst auf Nachfrage, dann mit Kürzeln.
3. Befehle je Projektart (CIDER bei Clojure, npm bei JavaScript), die
   meist eine Datei dieser Art voraussetzen.
4. Statt ein Menü abzusuchen: unscharfe Suche über die Befehle, die
   Kürzel daneben, damit man sie lernt.
5. Wenig verschiedene Mechanismen. Kein which-key; `C-h` nach einem
   Präfix ist die gewollte Hilfe. Lese-Menüs mit Schaltern (transient)
   nur in Werkzeugen, die sie mitbringen, wie magit.

**Einordnung: vier Arten, drei Mechanismen**

| Art der Auswahl | Beispiel | Mechanismus | Kürzel sichtbar über |
|---|---|---|---|
| Dynamische Daten | Dateien, Buffer, Projekte | vertico-Liste; RET tut das Übliche | — |
| Ausnahme zu einem Eintrag | Projekt → eshell, Magit; Datei → umbenennen; Buffer → schließen | embark auf dem Eintrag; die Aktionen sind eine Keymap | `C-h` in embark |
| Tastenfamilie | `M-c`, `M-v`, `C-c M-j` | Präfix-Map | `C-h` nach dem Präfix |
| Befehl suchen, Kürzel lernen | „wie startet hier die REPL?“ | `M-X`: Befehle, die zum Buffer passen | marginalia daneben |

Menüs sind damit immer Keymaps, Listen immer vertico, und die Hilfe ist
überall dieselbe: `C-h` öffnet eine durchsuchbare Liste mit Kürzel,
Befehl und Beschreibung.

**Vorschlag**

*A – Projektwechsel: Standard mit RET, Ausnahmen mit embark.*

- `C-x x f` (und `C-x p p`): Projektliste; RET öffnet die Dateiauswahl im
  Projekt, wie früher bei projectile (`project-switch-commands` auf
  `project-find-file`).
- Eine embark-Taste auf dem markierten Projekt (`M-x`, x wie execute;
  zuerst `M-o`) bietet die Ausnahmen an — **mit denselben Tasten wie
  die Projektbefehle auf `C-x x p`**: `C-x x p e` ist eshell im
  aktuellen Projekt, `M-x e` in der Liste eshell im markierten; einmal
  gelernt, an beiden Stellen gültig.
- `C-x x p` ist `jtk-project-map`: Emacs' `project-prefix-map` (`C-x p`)
  als Elternmap, darüber die eigenen Werkzeuge: `b`
  `consult-project-buffer`, `g` `consult-git-grep`, `v` und `m` Magit.
  `C-x p` selbst bleibt Emacs' Vorgabe.
- Projektarten: Ein Modul legt eine Keymap für seine Projektart an und
  meldet sie mit Markerdateien an (`project.clj`, `deps.edn` …).
  embark nimmt sie unter `j` in die Aktionen auf, nur bei Projekten
  dieser Art. Neben jedem Projekt in der Liste steht seine Art.
- `jtk-project-act` und `jtk-project-actions` aus 2.3 fallen wieder weg;
  die Daten stecken dann in Keymaps.

*B – Projektart-Befehle im Buffer: dieselbe Keymap unter einem Präfix.*

- Die Keymap der Projektart liegt in den Buffern dieser Art unter einem
  für alle Sprachen gleichen Präfix, Vorschlag `C-c M-j` — dort, wo
  CIDER heute schon seine Startfamilie hat (`J j S s B b`). `M-x j J`
  in der Projektliste ist dann `C-c M-j J` im Buffer.
- Weitere Arten in 2.8: npm (`package.json`), PlatformIO
  (`platformio.ini`), Python.

*C – Befehle suchen, Kürzel lernen.*

- `M-X` (eingebaut, schon belegt) zeigt die Befehle, die zum Buffer
  passen; marginalia schreibt Kürzel und Beschreibung daneben. Kein Code.
- Wahlweise `consult-mode-command`: alle Befehle der aktiven Modi, auch
  die ohne Kürzel, gruppiert nach Haupt- und Nebenmodus.
- Wahlweise `embark-bindings` auf `C-h b`: alle belegten Tasten des
  Buffers als durchsuchbare Liste statt als Hilfe-Buffer.

**Was der Prototyp zeigte** (Batch, Emacs 31.1, Wegwerfcode; die
embark-Taste war damals noch `M-o`):

| Eingabe in der Projektliste | Ergebnis |
|---|---|
| Clojure-Projekt, RET | `project-find-file` im Clojure-Projekt |
| Clojure-Projekt, `M-o e` | `*clj-eshell*` im Clojure-Projekt |
| anderes Projekt, `M-o m` | Magit im anderen Projekt |
| Clojure-Projekt, `M-o j` | jack-in im Clojure-Projekt |
| Projekt ohne `project.clj`, `M-o j` | keine Aktion: `j` gibt es dort nicht |

Technisch trägt das so (alles gegen den Quelltext geprüft):

- Die Kandidaten der Projektliste haben die Kategorie `project-file`;
  embark behandelt sie heute als Dateien. Über
  `marginalia-command-categories` bekommen sie eine eigene Kategorie.
- Ein embark-Transformer bestimmt die Projektart.
- `embark-keymap-alist` setzt die Aktionen aus mehreren Keymaps
  zusammen.
- Ein `:always`-Hook in `embark-around-action-hooks` lässt die Aktion
  im markierten Projekt laufen (`project-current-directory-override`,
  `default-directory`). embark selbst macht es mit `embark--cd` für
  shell und eshell genauso.
- **Mehrere Arten zugleich** (Clojure mit `package.json`) braucht einen
  kleinen Kniff, weil der Transformer nur einen Typ liefert. Lösung:
  den Typ aus den Arten zusammensetzen und seine Keymaps beim ersten
  Mal eintragen.

**Gegenvorschläge**

| Variante | Wie | Dafür | Dagegen |
|---|---|---|---|
| Liste nach jedem Wechsel (Stand 2.3) | wie jetzt, Kürzel in der Anzeige ergänzen | nichts Neues | der Normalfall kostet immer einen Schritt |
| Zwei Befehle, ohne embark | `C-x x f` Projekt + Datei; eine zweite Taste Projekt + Aktionsliste | am einfachsten, keine embark-Taste | man muss vor der Projektwahl wissen, dass man die Ausnahme will; die Aktionsliste ist ein eigenes Datenformat neben den Keymaps |
| embark, Projektarten nur über eine Liste | statische Aktions-Keymap; `j` öffnet eine Suchliste der Befehle dieser Art | kein Transformer-Kniff | eine Stufe mehr; im Buffer anders als in der Liste |
| transient-Projektmenü | magit-artig | Gruppen, Überschriften | Lese-Menü ohne Suche; bei einem Menü ohne Schalter kein Gewinn (verworfen) |
| which-key für alle Präfixe | Anzeige nach Verzögerung | ein Mechanismus für die Anzeige | nicht gewollt (verworfen) |
| project.els eigenes Menü | `project-switch-commands` als Liste | eingebaut | einzeilig im Echo-Bereich, ohne Suche (verworfen) |

**Entschieden (2026-10-29)**

| Nr. | Frage | Entscheidung |
|---|---|---|
| 1 | Weg A oder „zwei Befehle“ | Weg A mit embark |
| 2 | Taste für `embark-act` | zuerst `M-o`, dann `M-x` im Minibuffer (x wie execute; `M-x` wird sonst nur global gebraucht); im Buffer offen |
| 3 | Anzeige der Aktionen | nur auf `C-h` |
| 4 | `C-x p` umstellen | nein: eigene Map auf `C-x x p`, wie die übrigen Projekttasten unter `C-x x`; dort auch die Projektart-Befehle (`j`) |
| 5 | Präfix im Buffer | `C-c M-j` in allen Sprachen mit Projektart-Befehlen |
| 6 | Befehlssuche | `M-X` wie eingebaut, dazu `embark-bindings` auf `C-h b`; kein `consult-mode-command` |
| 7 | Wohin | in PR #4 |

**Umgesetzt, mit einer Abweichung vom Plan:** `j` ist kein eigener
embark-Typ je Projektart, sondern ein Befehl
(`jtk-project-run-kind-command`), der die Tasten der Projektart erst
liest, wenn das Projekt schon feststeht. Damit gibt es einen Weg für
die Liste und für `C-x x p`, und mehrere Arten zugleich brauchen keinen
Kniff: Ihre Keymaps werden zusammengesetzt. `C-h` nach `j` zeigt sie als
durchsuchbare Liste.

| Projektart | Marker | Tasten (unter `j` und `C-c M-j`) |
|---|---|---|
| Clojure | `project.clj`, `deps.edn`, `shadow-cljs.edn`, `bb.edn` | `jtk-clojure-project-map`: `J` jack-in, `j` connect, `S`/`s` ClojureScript, `B`/`b` beides — wie bisher unter `C-c M-j` |
| npm | `package.json` | npm-modes eigene Map (`i` install, `r` run, `s` install --save, …), dieselbe wie unter npm-modes `C-c n` |
| PlatformIO | `platformio.ini` | platformio-modes eigene Map (`b` build, `u` upload, `m` monitor, …), dieselbe wie unter `C-c i` |

- Bei gleicher Taste gewinnt die zuerst angemeldete Art (Clojure vor npm
  vor PlatformIO). In einem ClojureScript-Projekt mit `package.json` ist
  `j s` also CIDERs connect, nicht npms install --save.
- Python und TypeScript bekommen ihr `C-c M-j` in 2.8; in TypeScript
  belegt tide die Taste heute mit `tide-restart-server`.
- Nebenbei behoben: `cider-jack-in-dwim` (`J`) las die Profile immer aus
  `project.clj` und brach in Projekten ohne diese Datei ab; jetzt startet
  es dort das übliche jack-in.

**Geprüft** (Batch, Emacs 31.1, Tastenfolgen über
`execute-kbd-macro`; die Zielbefehle protokollierten nur, wo sie
liefen; alle Zeilen mit `M-x`):

| Eingabe | lief in |
|---|---|
| `C-x x f` Clojure-Projekt RET | `project-find-file` im Clojure-Projekt |
| `C-x x f` Clojure-Projekt `M-x e` | `*clj-eshell*` |
| `C-x x f` Clojure-Projekt `M-x v` / `M-x g` (aus einem anderen Projekt heraus) | Magit / `consult-git-grep` im Clojure-Projekt |
| `C-x x f` Clojure-Projekt `M-x j J` | `cider-jack-in-dwim` im Clojure-Projekt |
| `C-x x f` npm-Projekt `M-x j i` | `npm-mode-npm-install` im npm-Projekt |
| `C-x x f` ClojureScript + npm `M-x j s` / `M-x j i` | `cider-connect-cljs` / `npm-mode-npm-install` |
| `C-x x f` PlatformIO-Projekt `M-x j b` | `platformio-build` |
| `C-x x f` Projekt ohne Art `M-x j` | Meldung „No commands for this kind of project“ |
| `C-x x f` Clojure-Projekt `M-x j C-h` | die durchsuchbare Liste (`embark-bindings-in-keymap`) |
| `C-x x f` Clojure-Projekt `M-x C-h @ e` | eshell: `@` und Taste wirken auch in der Liste |
| in einer `.clj`-Datei `C-x x p j J`, `C-c M-j J`, `C-x x p g` | jack-in bzw. grep im Clojure-Projekt |
| in einer `.clj`-Datei `C-h b @ C-c M-j J`, `C-x x p j C-h @ J` | jack-in im Clojure-Projekt |

Neben jedem Projekt in der Liste stehen seine Arten („Clojure, npm“).

**Befehlssuche (C), entschieden für `M-X` und `C-h b`:**

| Weg | Was er zeigt | Dafür | Dagegen |
|---|---|---|---|
| `M-X` (eingebaut, belegt) | Befehle, die an einen Modus des Buffers gebunden sind oder in einer seiner Keymaps liegen | kein Code; Kürzel und Beschreibung über marginalia | was ein Paket weder bindet noch einem Modus zuordnet, fehlt; Keymaps, die überall aktiv sind (smartparens), füllen die Liste |
| `consult-mode-command` | alle Befehle aus den Dateien der aktiven Modi, auch ohne Kürzel, gruppiert nach Haupt- und Nebenmodus | vollständig je Paket (alle `cider-…`); eingrenzbar | braucht eine Taste; die Zuordnung über Dateien ist eine Heuristik; Befehle globaler Nebenmodi nur nach Eingrenzen |
| `embark-bindings` auf `C-h b` | alles, was im Buffer eine Taste hat | genau das Werkzeug zum Kürzel-Lernen; ersetzt den langen Hilfe-Buffer | Befehle ohne Taste fehlen |

**Kürzel direkt in Listen:** In jeder Liste, die `C-h` öffnet
(Aktionen nach `M-x`, Tasten nach einem Präfix, `C-h b`), führt `@` und
dann die Taste den Befehl direkt aus. Ohne `@` filtern Buchstaben die
Liste; beides auf denselben Tasten geht nicht.

**Exportierte Listen:** Was `C-o` in einen embark-Buffer exportiert
(etwa die Projektliste; Dateien, Zeilen und grep-Treffer gehen nach
dired, occur und grep), nimmt die Aktionstasten direkt auf dem Eintrag
unter dem Cursor an, wie nach `M-x` im Minibuffer (embarks
Direktmodus, immer an; `M-a` schaltet ihn ab). Geprüft im Batch auf der
exportierten Projektliste, aus einem anderen Projekt heraus: `e`
eshell, `v` Magit, `j J` jack-in, `j i` npm install, jeweils im
Projekt des Eintrags; `C-h` listet die Aktionen.

**Gefunden beim Umbau, älter als 2.3:** CIDER legt `cider-jack-in-clj`
in `clojure-mode-map` zweimal auf `C-c M-j`: aus seinen Autoloads, wenn
clojure-mode lädt, und noch einmal, wenn cider.el lädt. Die Familie
`J j S s B b` stand in CIDERs `:bind` und verlor gegen das zweite Mal.
Auf master war `C-c M-j` deshalb in jedem Clojure-Buffer CIDERs
normales jack-in, die Familie war nie erreichbar — geprüft im Batch mit
einer Spur aller `define-key` auf diese Taste. Jetzt wird sie nach
beiden Malen gesetzt. Der Schnappschuss konnte das nicht zeigen: Er
lädt alle Pakete auf einmal und sieht nur den Endzustand.

**Reihenfolge**

1. ✅ A und B für Clojure, npm und PlatformIO.
2. ✅ C: `embark-bindings` auf `C-h b`.
3. B für Python und TypeScript im jeweiligen Review in 2.8.

#### ✅ 2.4 Fenster und Arbeitsbereiche (entschieden und umgesetzt 2026-10-29)

| Vorher | Nachher | Warum |
|---|---|---|
| elscreen (`M-'`) | `tab-bar-mode`; `M-'` ist `jtk-tab-map` mit Emacs' `C-x t` als Elternmap | elscreen hat seit 2018 keinen Commit. |
| winner, `C-M-#` über `winner-undo` | `tab-bar-history-mode` auf denselben Tasten (`C-c ←`/`→`); `C-M-#` (`jtk-window-toggle-single`) merkt sich die verdeckten Fenster selbst | winner führt einen Verlauf je Frame und brächte nach einem Tab-Wechsel die Fenster des anderen Tabs zurück. |
| buffer-move (`C-M-'` + Pfeil) | `windmove-swap-states-*` auf denselben Tasten | Eingebaut, gleiches Verhalten: Der Cursor wandert mit dem Buffer. `buffer-move-stay-after-swap` war gesetzt, die installierte Fassung liest es aber nicht. |
| fullframe für magit | eigene `magit-display-buffer-function` (`jtk-vc-display-buffer`), `q` über `magit-restore-window-configuration` | Ein Paket weniger; das `:requires fullframe` an magit fällt weg. |
| `jtk-ide` (172 Zeilen) | eingebaute Seitenfenster in `display-buffer-alist`; `jtk-window-open-side-windows` statt `jtk-ide-setup` | Dieselbe Aufteilung; neu: `C-x w s` blendet alle Seitenfenster aus und ein. |
| `jtk-window-mirror` | entfernt | Nicht mehr benutzt. |
| ediff über Advice auf drei Befehle | Tab je Sitzung über `ediff-before-setup-hook` und `ediff-quit-hook` | Siehe unten. |
| ztree mit `:demand t` | lädt bei `ztree-diff` | `:demand` widersprach `:commands` (U3). |
| ace-window | bleibt | Gepflegt, kein Ersatz nötig. |

**Tasten unter `M-'`:** `c` neuer Tab, nach dem Projekt benannt; `C`
Tab kopieren; `k` schließen, `K` schließen samt den Buffern seiner
Fenster; `n`/`p` wechseln (wiederholbar, auch `o`/`O`), `N` zuletzt
benutzter Tab; `t` umbenennen; `f` per Auswahl. Dazu alles aus
`C-x t`, etwa `u` (geschlossenen Tab wiederholen) und `p` (Projekt in
neuem Tab). Die Leiste bleibt ausgeblendet (nach der Sichtung so
entschieden); die Namen zeigt `M-' f`.

**Tab-Namen:** Ein Tab aus `c` behält den Namen seines Projekts, auch
wenn darin Fenster anderer Projekte offen sind. Ein Tab ohne Namen
(`C`, `C-x t 2`, ediff) zeigt den Buffer des ausgewählten Fensters und
wechselt mit ihm.

**Gefunden beim Umbau, älter als 2.4:**
- Die Advice auf `ediff-quit` schloss den Screen auch, wenn man „Quit
  this Ediff session?“ mit `n` beantwortete. `ediff-quit-hook` läuft nur
  beim wirklichen Ende. Neu: Auch `magit-ediff` und `vc-ediff` bekommen
  einen Tab.
- `C-M-#` über einen Verlauf hätte mit `tab-bar-history` jedes zweite
  Mal zu weit zurückgesprungen: Der Verlauf überspringt denselben Befehl
  mehrmals hintereinander.

**Geprüft im Batch:** Tasten und Wiederholung unter `M-'`; `K` tötet
die Buffer, verweigert den letzten Tab; ediff öffnet einen Tab, `n`
lässt ihn stehen, `y` schließt ihn. `C-M-#` viermal hintereinander
wechselt zwischen einem und drei Fenstern, auch über einen Tab-Wechsel
hinweg. magit-status und beide Logs füllen den Frame, `q` bringt zwei
Fenster zurück, ein Diff aus dem Status erscheint daneben. Help und
Flycheck landen rechts (Slot 0 und 1), eshell und compilation unten;
`C-x 1` lässt sie stehen, `C-x o` überspringt sie, `C-x w s` blendet sie
aus und ein. Schnappschuss-Diff: nur die erwarteten Tasten, Hooks und
Optionen.

**Gesichtet in der GUI:** Die Größen der Seitenfenster (30 % der Höhe
bzw. Breite; vorher 300 bzw. 500 Pixel) passen. Die Tab-Leiste war
eingeblendet und ist wieder aus. Das „IDE“-Segment der Modeline
erscheint in den Seitenfenstern.

#### ✅ 2.5 Editieren (entschieden und umgesetzt 2026-10-29)

| Vorher | Nachher | Warum |
|---|---|---|
| undo-tree (`C-/`, `C-?`, `M-_`, `C-x u`) | eingebautes Undo, `undo-redo` auf `C-?` und `M-_`, vundo auf `C-x u` | undo-tree hat seit 2021 keinen Commit; Wiederholen kann Emacs seit 28.1. vundo: letzter Commit 2026-08. |
| aggressive-indent über `prog-mode-hook` | global ab dem Start | Der Hook rief den globalen Modus auf (U3): aus bis zum ersten Programmier-Buffer, danach überall. |
| highlight-symbol | symbol-overlay auf denselben Tasten (`C-m`, `M-m n/p/r`, Hervorheben nach 1,5 s) | highlight-symbol hat seit 2016 keinen Commit; symbol-overlay: 2026-07. |
| smartparens, Sprachregeln einzeln von Hand | `smartparens-config` lädt sie je Modus | Siehe unten. |
| `delete-trailing-whitespace` vor jedem Speichern | nur in `prog-mode` | In Markdown sind zwei Leerzeichen am Zeilenende ein Umbruch. |
| `cider-eval-dwim` mit `paredit-backward-up` | `backward-up-list` (eingebaut) | paredit kam nur als Abhängigkeit von clj-refactor mit. |
| multiple-cursors, kmacro, narrow, delete-selection | bleiben | multiple-cursors gepflegt (2026-10), der Rest eingebaut bzw. eigen. |

**Gefunden beim Umbau, älter als 2.5:**
- In Lisp wurde `'` zu `''` und `` ` `` zu ``` `` ``` gepaart, weil die
  Sprachregeln von smartparens fehlten. `smartparens-config` lädt sie
  jetzt; in JS und Python bleiben beide gepaart.
- `M-"` (Einfassen in doppelte Anführungszeichen) brach mit einem Fehler
  ab und hinterließ `(foo`: `sp-rewrap-sexp` nimmt ein Paar, keinen
  String. Jetzt über `sp-wrap-with-pair`.
- Nach `undo-redo` hätte aggressive-indent eingerückt und damit eine
  neue Änderung erzeugt; seine Schutzliste kannte nur undo-trees Befehle.

**Umbenannt:** Die eigenen Befehle standen im Namensraum von
smartparens: `sp-forward-transpose-sexp`, `sp-backward-transpose-sexp`,
`sp-wrap-doublequote` heißen jetzt `jtk-structural-editing-…`, auch in
`etc/mc-list.el`.

**Nebenwirkung von `smartparens-config`:** In Clojure gehören Präfixe
wie `#`, `'` und `@` jetzt zum Ausdruck, Sprünge und Slurp nehmen sie
mit (`sp-sexp-prefix`).

**Geprüft im Batch:** `C-/ C-/` und `C-?` in einem Elisp-Buffer, `C-x u`
öffnet vundo; aggressive-indent ist gleich nach dem Start in Elisp und
CSS an, in Python, Markdown, conf und fundamental aus; `M-m n` springt
zum nächsten Vorkommen, `C-m` markiert, ein danach getipptes `n` wird
eingefügt; Paarung von `' ` ( "` in Elisp, Clojure, JS und Python;
Vertauschen mit `C-M-<up>`/`<down>` und `M-"`; Leerzeichen am
Zeilenende fallen beim Speichern in `.el` weg, in `.md` und `.txt`
nicht; `cider-eval-dwim` wertet in vier Fällen (in `comment`, in einem
String, ohne `comment`, `deftest`) dieselbe Form aus wie mit paredit.
Schnappschuss-Diff: nur die erwarteten Tasten, Hooks und Optionen.

**Gesichtet in der GUI:** `M-_` fehlte als Wiederholen und ist wieder
da. Die Farben von symbol-overlay wirken schwächer als früher; sie
bleiben vorerst, wie das Theme sie vorgibt. Offen: die vundo-Ansicht.

#### ✅ 2.6 Prüfen, Vervollständigen im Buffer, Snippets (entschieden und umgesetzt 2026-10-29)

| Vorher | Nachher | Warum |
|---|---|---|
| company in `prog-mode` und der CIDER-REPL, company-quickhelp | corfu überall (`global-corfu-mode`), Doku über `corfu-popupinfo`, Verlauf über `corfu-history` | corfu setzt auf `completion-at-point` und `completion-styles` auf, im Buffer gilt also dieselbe orderless-Suche wie im Minibuffer. Letzter Commit 2026-10. |
| — | cape: Dateinamen und Wörter der offenen Text- und Code-Buffer, wo der Modus nichts anbietet | Gewünscht: Begriffe des Buffers und zuletzt getippte. cape: 2026-09. |
| elpy (Python) | entfernt, mit highlight-indentation und pyvenv | Python wird nicht mehr benutzt. `.py` öffnet Emacs' eigenes `python-mode`. |
| yasnippet-snippets | entfernt | Nicht benutzt; die eigenen Vorlagen bleiben. |
| flycheck, flycheck-clj-kondo | bleiben bis 2.7 | Entschieden: flymake, aber zusammen mit eglot, damit keine Zwischenpakete entstehen. |
| yasnippet | bleibt | Gepflegt; eglot nutzt es für Vorlagen des Language-Servers. |

**Verhalten von corfu:**
- Das Popup erscheint von selbst nach 0,2 s, sobald drei Zeichen getippt sind (wie bei company).
- `TAB` vervollständigt, `RET` übernimmt, `C-g` bricht ab; die Doku erscheint nach 0,2 s.
- Schon einmal gewählte Einträge stehen oben und bleiben über savehist erhalten.
- Emacs 30 bietet in Text-Modi Wörter aus dem Rechtschreibwörterbuch an; das ist aus (`text-mode-ispell-word-completion`).

**Woher die Vorschläge kommen:**
- Zuerst fragt corfu die Vervollständigung des Modus: Elisp, CIDER mit verbundener REPL, eshell.
- Nur wo diese nichts liefert, antworten Dateinamen und dann Wörter aller offenen Text- und Code-Buffer, der aktuelle zuerst. Das gilt in Text, Org, Kommentaren und in Clojure ohne REPL.
- Im Code mischen sich die Wörter also nicht unter die Symbole des Modus. Ginge mit `cape-capf-super`, ist aber nicht eingebaut.

**company bleibt installiert,** als Abhängigkeit von esh-autosuggest, indium und tide, wird aber nirgends eingeschaltet. esh-autosuggest nutzt es in eshell nur für den Vorschlag aus dem Verlauf.

**Geprüft im Batch:**
- Vorschlagsquelle je Modus:
  - Text und Org: Wörter, auch aus einem anderen Text-Buffer;
  - Elisp: Symbole im Code, Wörter im Kommentar;
  - Clojure ohne REPL: Wörter;
  - Text mit Pfad: Dateinamen.
- corfu, popupinfo und history sind an, `corfu-history` steht in `savehist-additional-variables`.
- Die eigenen Vorlagen kommen an: 7 in Clojure, 7 in web-mode.
- Schnappschuss-Diff: weg sind die Tasten von elpy und python sowie die von ido und hideshow (elpy zog beide mit); dazu kommen die Tasten und Optionen von corfu und cape.
- corfu selbst schaltet sich im Batch nicht ein (`noninteractive`); das Popup zeigt erst die GUI.

**Gesichtet in der GUI:** Popup und Doku, Vorschläge aus Code,
Kommentar, Org und der CIDER-REPL, corfu in eshell neben dem
Verlaufsvorschlag von esh-autosuggest — alles in Ordnung.

#### ✅ 2.7 Sprach-Infrastruktur (entschieden und umgesetzt 2026-10-29)

| Thema | Entschieden | Warum |
|---|---|---|
| LSP-Client | **eglot** (eingebaut), Grundeinstellungen in `init-lsp` | Liefert an xref, eldoc, `completion-at-point` und flymake, also an die Tasten und Oberflächen, die alle Sprachen schon haben. Verworfen: lsp-mode (eigene Welt, viel Konfiguration). |
| Server | nur **clojure-lsp** (vorhanden, per brew), in jedem Clojure-Buffer neben CIDER | Gearbeitet wird fast nur in Clojure/ClojureScript. |
| Prüfen | **flymake** statt flycheck, überall in `prog-mode`; flycheck-clj-kondo entfällt | eglot meldet nur an flymake; clj-kondo kommt über clojure-lsp. |
| tree-sitter | eingebaut über `treesit-enabled-modes`; zunächst JSON, CSS, Shell, YAML, TOML, Dockerfile | Emacs 31 leitet die Modi um und installiert Grammatiken auf Nachfrage. Verworfen: treesit-auto (überflüssig). |

**eglot:**
- Ein Server endet mit seinem letzten Buffer; das Protokoll-Log ist aus.
- Tasten: `M-r R` umbenennen, `M-r A` Code-Aktionen, `M-r O` Importe ordnen, `M-r =` formatieren.
- Großbuchstaben, weil clj-refactor in Clojure fast alle Kleinbuchstaben nach `M-r` belegt.

**flymake:**
- `C-c ! n`/`p` springen zum nächsten bzw. vorigen Befund, wiederholbar; `C-c ! l` listet die Befunde im Buffer, `C-c ! L` im Projekt.
- Die Modeline zählt die Befunde über ein eigenes spaceline-Segment, weil spaceline nur flycheck kennt.
- **Vertrauenswürdige Dateien:** Elisp prüft flymake mit dem Byte-Compiler nur in Dateien, denen Emacs vertraut (`trusted-content`). Sonst läuft nur checkdoc, still: Im Batch stand das nur im `*Flymake log*`. Jetzt sind `early-init.el`, `init.el`, `modules/` und `lisp/` eingetragen, die Pakete unter `elpaca/` nicht.

**tree-sitter:** Ohne Grammatik öffnet ein tree-sitter-Modus ohne Hervorhebung. Emacs fragt deshalb bei der ersten Datei einer Art, ob er die Grammatik installieren soll; das braucht einen C-Compiler, auf dem Mac die Xcode-Command-Line-Tools. SCSS, Clojure und Markdown bleiben bei ihren Modi: Für SCSS und Clojure bringt Emacs keinen tree-sitter-Modus mit, für Markdown ist markdown-mode eingerichtet.

**Geprüft im Batch:**
- **clojure-lsp 2026.10.15** an einem kleinen `deps.edn`-Projekt:
  - Befunde von clj-kondo (Namespace ungenutzt, Binding ungenutzt, unbekanntes Symbol) und von clojure-lsp selbst (öffentliche Var ungenutzt);
  - `M-j d` findet die Definition;
  - Vervollständigen bietet die Funktionen des Projekts an.
  - `eglot-ensure` startet den Server erst nach dem nächsten Befehl, im Batch also erst nach `post-command-hook`.
- **flymake in Elisp:** checkdoc überall, Byte-Compiler-Befunde in der eigenen Konfiguration; `C-c ! n` und die Wiederholung funktionieren.
- **tree-sitter:** Die sechs Dateiarten öffnen im tree-sitter-Modus; SCSS, Clojure und Markdown bleiben bei ihren Modi.
- **Schnappschuss-Diff:** nur eglot, flymake, die Umleitungen und die Hooks.

**Für 2.8 entschieden:** Außer Clojure/ClojureScript, SCSS, etwas CSS, HTML und JS, JSON, Jenkinsfile, Shell und Markdown wird nichts gebraucht; der Rest kann weg. Neu gewünscht: ein ordentliches Setup für XML und XSLT. Wichtig ist die Leistung in großen Dateien.

**Offen für die Sichtung in der GUI:** clojure-lsp im echten Projekt neben CIDER (Vervollständigen, `M-j d`, Befunde in der Modeline); die Nachfrage nach der Grammatik beim ersten JSON- oder CSS-File.

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
| smartrep | M | Bootstrap | Wiederholtasten | **ersetzt** (2.2) durch `repeat-mode` | Ja |
| exec-path-from-shell | M | Bootstrap | PATH aus Login-Shell | installiert, **nie benutzt** — einsetzen | |
| doom-themes | M | apperance | Theme `doom-one` | gepflegt | |
| spaceline | M | apperance | Modeline | kaum noch gepflegt | |
| linum | e | apperance | Zeilennummern | obsolet seit 29.1 | Ja |
| rainbow-mode | M | apperance | Farbwerte einfärben | ok; in jedem `prog-mode` fraglich | Ja |
| rainbow-delimiters | M | apperance | Klammern einfärben | ok | Ja |
| kmacro | e | interface | Makros | ok | |
| ivy / counsel / swiper | M | interface | Vervollständigung, Suche | **ersetzt** (2.1) durch vertico, orderless, marginalia, consult, embark, embark-consult | Ja |
| winner | e | interface | Fensterlayout zurück | **ersetzt** (2.4) durch `tab-bar-history-mode` (Verlauf je Tab) | Ja |
| fullframe | M | interface | magit im Vollbild | **ersetzt** (2.4) durch eine eigene `magit-display-buffer-function` | Ja |
| buffer-move | M | interface | Buffer tauschen | **ersetzt** (2.4) durch `windmove-swap-states-*` | Ja |
| elscreen | M | interface | Bildschirme je Projekt | **ersetzt** (2.4) durch `tab-bar-mode`; letzter Commit 2018 | Ja |
| ediff | e | interface | Vergleich | ok | Ja |
| ztree | M | interface | Verzeichnisvergleich | bleibt (2.4), gepflegt; lädt erst bei Bedarf | Ja |
| ace-jump-mode | M | interface | Springen | **ersetzt** (2.2) durch avy, das ace-window ohnehin mitbringt. Die frühere Einschätzung „ungepflegt“ war falsch: Es bekommt 2026 noch Commits | |
| ace-window | M | interface | Fensterwahl | gepflegt | Ja |
| expand-region | M | interface | Auswahl erweitern | **ersetzt** (2.2) durch expreg | Ja |
| which-key | M | interface | Tastenhilfe | **entfernt** (2026-10-25): das Rezept klonte das ganze Emacs-Repository, der erste Start hing; nicht mehr gebraucht | |
| undo-tree | M | interface | Undo-Baum | **ersetzt** (2.5) durch `undo-redo` und vundo | Ja |
| multiple-cursors | M | interface | Mehrfach-Cursor | bleibt (2.5), gepflegt | Ja |
| counsel-projectile / projectile | M | filemanagement | Projekte | counsel-projectile **entfernt** (2.1); projectile **ersetzt** (2.3) durch project.el, bleibt aber als Abhängigkeit von platformio-mode installiert | Ja |
| git-auto-commit-mode | M | filemanagement | Auto-Commit (wofür?) | prüfen | |
| treemacs (+ -icons-dired, -magit) | M | filemanagement | Dateibaum | gepflegt; treemacs-projectile **entfernt** (2.3), treemacs fragt project.el | Ja |
| simple-bookmarks | M? | filemanagement | Lesezeichen, MySQL-Verbindungen | eigenes Paket (eigenes Repo), womöglich veraltet — Review wie ein Fremdpaket | Ja |
| org | e | organisation | GTD | bleibt | |
| dashboard (+ page-break-lines) | M | startup | Startbildschirm | gepflegt | |
| smartparens | M | development | Klammern, sexp-Editieren | bleibt (2.5), gepflegt; Sprachregeln über `smartparens-config` | Ja |
| aggressive-indent | M | development | Einrücken beim Tippen | bleibt (2.5), global ab dem Start | Ja |
| company / company-quickhelp | M | development | Vervollständigung im Buffer | **ersetzt** (2.6) durch corfu und cape; company bleibt als Abhängigkeit installiert | Ja |
| magit | M | development | Git | bleibt | Ja |
| flycheck | M | development | Syntaxprüfung | **ersetzt** (2.7) durch flymake; bleibt als Abhängigkeit von tide installiert | Ja |
| yasnippet / yasnippet-snippets | M | development | Snippets | yasnippet bleibt (2.6); yasnippet-snippets **entfernt**, nicht benutzt | Ja |
| highlight-symbol | M | development | Symbol hervorheben | **ersetzt** (2.5) durch symbol-overlay | Ja |
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
| flycheck-clj-kondo | M | clojure | Linter | **entfernt** (2.7), clj-kondo kommt über clojure-lsp | |
| js2-mode / js2-refactor / xref-js2 | M | javascript | JS | → `js-ts-mode` + eglot | |
| indium | M | javascript | JS-Debugger | Pflegestand prüfen | |
| tide / npm-mode | M | typescript | TS | → `typescript-ts-mode` + eglot | |
| web-mode | M | html | HTML/XML/XSL | gepflegt | |
| web-mode-edit-element | M | html | strukturelles HTML-Editieren | eigenes Paket (eigenes Repo), womöglich veraltet — Review wie ein Fremdpaket | |
| scss-mode / sass-mode / css-mode | M/M/e | css | Stylesheets | scss-mode überflüssig | Ja |
| elpy | M | python | Python | **entfernt** (2.6), Python wird nicht mehr benutzt | |
| c++-mode / platformio-mode | e/M | c | Arduino/PlatformIO | prüfen | |
| basic-mode | M | basic | ASP/BASIC | Fehler B6 | |
| php-mode / geben / company-php | M | php (inaktiv) | PHP | auskommentiert — löschen? | |

### Neue Pakete (Kandidaten)

Noch nicht in der Konfiguration; aufgenommen auf deinen Wunsch
(2026-10-25). Entschieden wird im genannten Block.

| Paket bzw. Programm | Art | Block | wofür | Einschätzung |
|---|---|---|---|---|
| eglot | Emacs-Paket, eingebaut seit 29 | 2.7 | LSP-Client | **aufgenommen** (2.7) |
| lsp-mode (+ lsp-ui, dap-mode) | Emacs-Paket, MELPA | 2.7 | LSP-Client, Gegenkandidat | **verworfen** (2.7): eigene Welt, viel Konfiguration |
| flycheck-eglot | Emacs-Paket, MELPA | 2.6/2.7 | eglot-Befunde in flycheck | **nicht nötig** (2.7): flymake |
| emacs-lsp-booster (+ eglot-booster) | Programm + Emacs-Paket | 2.7, Phase 3 | schnellere JSON-Verarbeitung | nur, wenn eine Messung Bedarf zeigt |
| treesit-auto | Emacs-Paket, MELPA | 2.7 | `*-ts-mode` und Grammatiken automatisch | **verworfen** (2.7): Emacs 31 kann das selbst (`treesit-enabled-modes`, Installation auf Nachfrage) |
| clojure-lsp | Language-Server | 2.7 | Navigation, Refactoring, clj-kondo-Befunde | **aufgenommen** (2.7), neben CIDER; Überschneidung mit clj-refactor klärt 2.8 |
| clojure-ts-mode | Emacs-Paket, MELPA | 2.8 Clojure | tree-sitter-Modus für Clojure | Pflegestand und CIDER-Verträglichkeit prüfen |
| typescript-language-server | Language-Server (npm) | 2.8 JS/TS | JavaScript und TypeScript | ersetzt tide, xref-js2 und Teile von js2-refactor |
| basedpyright bzw. pyright, dazu ruff | Language-Server | 2.8 Python | Python | ersetzt elpy |
| vscode-langservers-extracted | Language-Server (npm) | 2.8 HTML/CSS | HTML, CSS/SCSS, JSON | scss ist als benutzt markiert |
| clangd | Language-Server | 2.8 C++ | C++, auch PlatformIO über `compile_commands.json` | |
| marksman | Language-Server | 2.8 Markdown | Links und Überschriften in Markdown | optional |

**WATCHOUT: Language-Server sind Programme außerhalb von Emacs.** Die
Lockdatei pinnt sie nicht; ihre Version kommt aus Homebrew bzw. npm.
Gleiche Versionen auf jedem Rechner (Q2) brauchen dafür einen eigenen
Ort, etwa ein Brewfile im Repo — zu entscheiden in 2.7. Und CI prüft
bisher nur Emacs-Pakete; ob die Action Server mitinstalliert, gehört
zur selben Entscheidung.

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
