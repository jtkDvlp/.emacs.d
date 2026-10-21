# CLAUDE.md – Emacs-Konfiguration

Arbeitsnotizen für die Zusammenarbeit an dieser Konfiguration. Es gelten die
projektübergreifenden Richtlinien aus `claude-guidelines`; was hier steht,
ergänzt sie oder weicht bewusst ab.

Der laufende Umbau ist in [`dev/plan.md`](dev/plan.md) beschrieben —
Bestandsaufnahme, Befunde, Zielbild und Reihenfolge. Diese Datei hält nur
fest, was dauerhaft gilt.

## Was das ist

Die persönliche Emacs-Konfiguration, geklont nach `~/.emacs.d`. Kein
veröffentlichtes Paket, also keine Versionierung und kein Release —
Conventional Commits trotzdem, damit die Historie lesbar bleibt.

- Zuletzt angepasst an Emacs 29.3 (Commit `835e12a`). Zielversion: offen,
  siehe Plan.
- Plattform: macOS (`mac-command-modifier`), vermutlich auch Linux/X.
- Schwerpunkt: Clojure/ClojureScript mit CIDER; daneben Elisp, JS/TS,
  HTML/CSS, Python, C++ (PlatformIO), Org (GTD).

## Aufbau (Ist-Stand)

| Pfad | Inhalt |
|---|---|
| `init.el` | Setzt `env-jira-host-url`, nimmt `configuration/` in den `load-path`, lädt `configuration`. |
| `configuration/configuration.el` | Paketsystem-Bootstrap, Grundeinstellungen, `require` aller Module in fester Reihenfolge, zwei Hilfsfunktionen (`dump`, `replace-keymap-bindings`). |
| `configuration/base--*.el` | Grundfunktionen, grob nach Bereich (Aussehen, Bedienung, Dateien, Org, Start, Entwicklung, Shell). |
| `configuration/lang--*.el`, `file--csv.el` | Je Sprache bzw. Dateityp eine Datei. |
| `configuration/pkg--*.el` | **Eigene Bibliotheken** ohne Seiteneffekte beim Laden (`ide`, `narrow`, `zoom`, `window-mirror`), per `use-package … :load-path` eingebunden. |
| `snippets/` | Handgeschriebene yasnippet-Vorlagen. |
| `eshell/alias` | Handgeschriebene eshell-Aliase. |
| `.mc-lists.el` | Von multiple-cursors **erzeugt**, aber inhaltlich gepflegte Vorlieben — deshalb versioniert. |

## Arbeitsweise in diesem Projekt

**Das Ergebnis ist das laufende Emacs.** Sichtung heißt: Emacs mit dem
geänderten Stand starten und das Verhalten ansehen — nicht den Diff lesen.
Damit das billig bleibt:

- Den Branch als eigenes Worktree auschecken und parallel zum Alltags-Emacs
  starten, ohne die laufende Konfiguration anzufassen (ab Emacs 29):
  ```
  git worktree add ~/emacs-next <branch>
  emacs --init-directory ~/emacs-next
  ```
  Pakete landen dann in `~/emacs-next/elpa/` — der erste Start dauert.
- Zu jedem Schritt gehört eine kurze Sichtungsliste: welche Tasten, welche
  Modi, welche Datei man öffnen soll, und was dabei passieren muss.

**In dieser Cloud-Umgebung ist kein Emacs installiert.** Aussagen über
Verhalten sind dort Lesart des Codes, nicht Beobachtung — das wird im
Bericht so gekennzeichnet.

**Startzeit wird gemessen, nicht geschätzt** (`M-x emacs-init-time`, für die
Aufschlüsselung `use-package-compute-statistics` auf `t` und
`M-x use-package-report`). Vor dem Umbau eine Baseline festhalten — siehe
Plan, Phase 0.

## WATCHOUT

Fallstricke im Ist-Stand, die beim Umbau nicht verloren gehen dürfen:

- **Ladereihenfolge entscheidet über globale Tasten.** `:bind*` schreibt in
  `override-global-map`; wer später lädt, gewinnt. So überschreibt
  `base--organisation` heute `C--` aus `base--interface` (siehe Plan, B1).
  Beim Neuschnitt der Module also die Reihenfolge der `require`s mitdenken.
- **`:bind*` mit `:map` wirkt wie `:bind`.** Die Belegung landet in der
  genannten Mode-Map, nicht in der Override-Map. Das Sternchen dort trägt
  nichts und täuscht Absicht vor.
- **`:requires` überspringt die ganze Deklaration**, wenn das Feature beim
  Auswerten noch nicht geladen ist — still, ohne Meldung. `:after` ist fast
  immer das Gemeinte.
- **`cider-eval-dwim` ruft `paredit-backward-up`**, installiert wird
  `paredit` aber nirgends ausdrücklich — es kommt meines Wissens als
  Abhängigkeit von `clj-refactor` mit. Fällt `clj-refactor` weg, bricht die
  Funktion.
- **`with-elscreen` wird in `elscreen`s `:init` definiert, aber von `ediff`
  benutzt.** Wer `elscreen` ersetzt, muss die `ediff`-Einbindung mitnehmen.
- **eshell-Tasten gehen über `eshell-mode-map*`**, weil eshell seine
  Mode-Map früher erst beim Start des Modus angelegt hat. Ob das in der
  Zielversion noch nötig ist, ist zu prüfen, bevor es wegfällt.

## Entscheidungen

Wird mit dem Umbau gefüllt — je Entscheidung: was, warum, was verworfen
wurde.

| Datum | Entscheidung | Begründung |
|---|---|---|
| — | — | — |
