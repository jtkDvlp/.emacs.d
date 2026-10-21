# CLAUDE.md – Emacs-Konfiguration

Arbeitsnotizen für die Zusammenarbeit an dieser Konfiguration. Es gelten die
projektübergreifenden Richtlinien; was hier steht, ergänzt sie oder weicht
bewusst ab.

@../claude-guidelines/CLAUDE.md

Der Pfad ist der in der Cloud-Umgebung, in der hier gearbeitet wird: Beide
Repos liegen dort nebeneinander unter `/home/user/`. Lokal auf dem Mac läuft
Claude derzeit nicht.

Der laufende Umbau ist in [`dev/plan.md`](dev/plan.md) beschrieben —
Bestandsaufnahme, Befunde, Zielbild und Reihenfolge. Diese Datei hält nur
fest, was dauerhaft gilt.

## Was das ist

Die persönliche Emacs-Konfiguration, geklont nach `~/.emacs.d`. Kein
veröffentlichtes Paket, also keine Versionierung und kein Release —
Conventional Commits trotzdem, damit die Historie lesbar bleibt.

- Zuletzt angepasst an Emacs 29.3 (Commit `835e12a`). Zielversion: die
  jeweils neueste stabile Emacs-Version.
- Plattform: **nur macOS.** Fallunterscheidungen für Linux, X oder Windows
  (`windows-nt` bei `sql`, `x` bei der PATH-Übernahme) tragen nichts und
  fallen beim jeweiligen Umbau weg.
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
| 2026-10-21 | **Ziel ist die neueste stabile Emacs-Version, nur macOS.** | So sind die eingebauten Nachfolger (which-key, eglot, tree-sitter, `use-package :vc`) ohne Zusatzpakete da. Andere Plattformen werden nicht benutzt. |
| 2026-10-21 | **use-package bleibt** als Deklarationsform, wird aber idiomatisch benutzt (Konventionen im Plan, Abschnitt 2). | Eingebaut, Standard, deklarativ. Verworfen: reines Elisp (mehr Zeilen für dieselbe Aussage), `leaf`/`setup.el` (Nische), Literate Config (Übersetzungsschritt, Fehler zeigen auf die erzeugte Datei), Starter-Kits (überschreiben das eigene Tastaturkonzept). |
| 2026-10-21 | **Paketverwaltung mit Lockdatei: elpaca** statt package.el. | Gleiche Paketversionen sind wichtig; package.el kennt keine Lockdatei. elpaca hält jedes Paket als Git-Klon und schreibt die Commits in eine Lockdatei, die versioniert wird; eigene Pakete kommen über ein Rezept direkt aus ihrem Repo. Verworfen: `straight.el` (dasselbe Prinzip, älter; meines Wissens vom elpaca-Autor nur noch mitgepflegt), package.el + `package-vc` (keine Lockdatei). |
| 2026-10-21 | **Zielbild aus dem Plan, Abschnitt 4:** `modules/init-<thema>.el` für Konfiguration, `lisp/jtk-*.el` für eigene Bibliotheken, no-littering mit `etc/` (versioniert) und `var/` (nicht versioniert). | Bewusste Abweichung von `src/`/`resources/`/`target/` der Richtlinien: Emacs gibt `init.el` und `early-init.el` im Wurzelverzeichnis vor, und `src/jtkdvlp/<artifact>/` wäre eine Ebene, die nichts trennt. Die Rollen bleiben: `modules/` und `lisp/` sind Quellcode, `snippets/` und `eshell/` handgeschriebene Ressourcen, `var/` das Generierte. |
| 2026-10-21 | **`simple-bookmarks` und `web-mode-edit-element` sind eigene Pakete** und kommen aus ihren eigenen Repos. | Sie bekommen im Paket-Review dieselben Fragen wie Fremdpakete — sie sind womöglich veraltet. |
| 2026-10-21 | **Prüfskript unter `scripts/` und GitHub-Action** sind freigegeben. | Batch-Laden und Byte-Compile fangen Tippfehler und unbekannte Funktionen, die beim Lesen durchgehen; die Action macht das ohne lokales Emacs sichtbar. |
| 2026-10-21 | **Bekannte Fehler (Plan, B1–B7) werden im Review des jeweiligen Pakets behoben**, nicht vorgezogen. | Manche Pakete fallen dabei ganz weg; ein vorgezogener Fix wäre dann verlorene Arbeit. |
