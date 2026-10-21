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

Phase 1 des Plans ist bis Schritt 1.3 umgesetzt; die Module liegen noch in
`configuration/` und tragen ihre alten Namen.

| Pfad | Inhalt | versioniert |
|---|---|---|
| `early-init.el` | package.el aus, native Kompilate nach `var/`, Frame ohne Leisten und maximiert. | ja |
| `init.el` | elpaca-Installer (gepinnt über die Lockdatei), `elpaca-use-package`, no-littering, `local.el`, dann `configuration`. | ja |
| `configuration/configuration.el` | Pakete, die vor allen Modulen da sein müssen (`diminish`, `smartrep`), PATH, Grundeinstellungen, `require` aller Module in fester Reihenfolge, zwei Hilfsfunktionen (`dump`, `replace-keymap-bindings`). | ja |
| `configuration/base--*.el` | Grundfunktionen, grob nach Bereich (Aussehen, Bedienung, Dateien, Org, Start, Entwicklung, Shell). | ja |
| `configuration/lang--*.el`, `file--csv.el` | Je Sprache bzw. Dateityp eine Datei. | ja |
| `configuration/pkg--*.el` | **Eigene Bibliotheken** ohne Seiteneffekte beim Laden (`ide`, `narrow`, `zoom`, `window-mirror`), per `use-package … :ensure nil :load-path` eingebunden. | ja |
| `etc/elpaca-lock.eld` | Commit jedes Pakets (93, mit Abhängigkeiten und elpaca selbst). | ja |
| `etc/yasnippet/snippets/` | Handgeschriebene yasnippet-Vorlagen. | ja |
| `etc/eshell/aliases` | Handgeschriebene eshell-Aliase. | ja |
| `etc/mc-list.el` | Von multiple-cursors **erzeugt**, aber inhaltlich gepflegt — deshalb versioniert. | ja |
| `var/` | Zustand und Caches (no-littering), `custom.el`, native Kompilate. Löschbar; verloren gehen nur Verläufe. | nein |
| `elpaca/` | Paket-Klone und Builds. Löschbar; die Lockdatei stellt sie wieder her. | nein |
| `local.el` | Maschinenspezifisches, etwa `env-jira-host-url`. | nein |
| `scripts/check`, `scripts/check.el` | Prüfskript, siehe unten. | ja |
| `dev/plan.md` | Befund und Plan des Umbaus. | ja |
| `dev/cloud/` | Nur für die Cloud-Umgebung: Emacs-Aufruf mit Umgehung der gesperrten Hosts. | ja |

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

**`scripts/check` lädt die Konfiguration im Batch-Modus und
byte-kompiliert alle Dateien.** Der Bericht (Markdown) sagt, ob das Laden
klappte, und zählt je Datei Fehler und Warnungen; bei einem Ladefehler
kommt der Backtrace dazu. Es endet mit einem Fehler, wenn das Laden oder
eine Datei scheitert — Warnungen allein nicht, ihre Zahl ist das Maß. Die
GitHub-Action (`.github/workflows/check.yml`, macOS, Emacs 31.1) führt es
bei jedem Push aus und legt den Bericht in die Zusammenfassung des Laufs.
Ein anderes Emacs wählt man über `EMACS=/pfad/zu/emacs scripts/check`.

`--verify-lock` (so in CI) schreibt nach dem Laden die Lockdatei neu und
scheitert, wenn sich dabei ein Commit ändert — der Beweis, dass die
eingecheckte Lockdatei installiert, was sie sagt. `--write-lock` tut
dasselbe ohne zu scheitern und ist der Weg, die Lockdatei nach einem
Paket-Update fortzuschreiben (gleichwertig: in Emacs
`M-x elpaca-write-lock-file` auf `etc/elpaca-lock.eld`).

**Der Byte-Compiler fängt keine falschen Funktionsnamen in `:hook`,
`:bind` oder `:commands`** — dort stehen sie als Daten, nicht als Aufruf.
Fehler wie B3 und B4 aus dem Plan bleiben also auch mit grüner CI stehen.

**In der Cloud-Umgebung gibt es kein Emacs, bis man es baut.** Der Weg,
der funktioniert hat: Quellen per `git clone --depth 1 --branch
emacs-31.1` vom Spiegel `github.com/emacs-mirror/emacs` (die GNU-Server
sind über den Proxy gesperrt), Abhängigkeiten per `apt-get`, dann
`./configure --without-x --with-tree-sitter --without-native-compilation`
und `make`, nach `/opt/emacs-31.1`. Dauer rund 15 Minuten.

Erreichbar sind von dort GitHub und GitLab, nicht aber melpa.org,
elpa.gnu.org, Codeberg, Sourcehut und paredit.org. `dev/cloud/emacs`
überbrückt das: Die Metadaten der Archive werden leer beantwortet (elpaca
holt Rezepte und Quellen ohnehin von GitHub), die übrigen Hosts per
git-`insteadOf` auf ihre emacsmirror-Kopien umgeleitet — dieselben
Commits, also bleibt eine dort geschriebene Lockdatei gültig. Aufruf:
`EMACS=dev/cloud/emacs scripts/check`. Eine Erstinstallation aller Pakete
dauert so rund 6 Minuten.

Der dort gebaute Emacs hat keine GUI, die nix-Builds der Action auch
nicht. Aussagen über das Verhalten der GUI bleiben Lesart des Codes.

**Startzeit wird gemessen, nicht geschätzt** (`M-x emacs-init-time`, für die
Aufschlüsselung `use-package-compute-statistics` auf `t` und
`M-x use-package-report`). Vor dem Umbau eine Baseline festhalten — siehe
Plan, Phase 0.

## WATCHOUT

Fallstricke im Ist-Stand, die beim Umbau nicht verloren gehen dürfen:

- **elpaca installiert asynchron, nach `init.el`.** Ein `use-package` mit
  `:ensure t` (der Standard) wird erst danach ausgewertet. Code auf
  oberster Ebene, der ein Paket aufruft, läuft also vorher ins Leere —
  dafür gibt es `:ensure (:wait t)` (`diminish`, `smartrep`,
  `elpaca-use-package`, `no-littering`). Umgekehrt läuft ein Block mit
  `:ensure nil` **sofort**, auch wenn er weiter unten steht als ein
  Block, auf den er sich verlässt.
- **Eingebaute und lokale Pakete brauchen `:ensure nil`.** Sonst sucht
  elpaca ein Rezept (`pkg--*`, `sql`) oder holt eine zweite Fassung aus
  Git (`org`).
- **no-littering muss vor allen Paketen kommen, deren Pfade es setzt**, und
  es setzt nur Vorgaben: Ein späteres `setq` gewinnt. So bleiben
  `eshell-rc-script` (`~/.eshellrc`) und `mc/list-file` (`etc/`) bewusst
  abweichend.
- **Nach einem Paket-Update die Lockdatei fortschreiben**, sonst
  installiert ein neuer Rechner die alten Commits — und CI meldet die
  Abweichung erst, wenn sie gegen die Lockdatei prüft.
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
  `paredit` aber nirgends ausdrücklich — es kommt als Abhängigkeit von
  `clj-refactor` mit (bestätigt: ohne `paredit` scheitert dessen
  Installation). Fällt `clj-refactor` weg, bricht die Funktion.
- **`with-elscreen` wird in `elscreen`s `:init` definiert, aber von `ediff`
  benutzt.** Wer `elscreen` ersetzt, muss die `ediff`-Einbindung mitnehmen.
- **eshell-Tasten gehen über `eshell-mode-map*`**, weil eshell seine
  Mode-Map früher erst beim Start des Modus angelegt hat. Ob das in der
  Zielversion noch nötig ist, ist zu prüfen, bevor es wegfällt.

## Messwerte

| Stand | Laden | Pakete | Compile-Fehler (Anzahl) | Compile-Warnungen (Anzahl) |
|---|---|---|---:|---:|
| `a116aa1` (vor dem Umbau), CI, Emacs 31.1 ohne GUI | scheitert an `(scroll-bar-mode -1)` | — | — | — |
| nach Phase 1, Schritt 1.3, Cloud, Emacs 31.1 ohne GUI | ok | 93 installiert, Lockdatei reproduziert | 0 | 52 |

Die Startzeit in der GUI fehlt noch (Plan, Phase 0).

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
| 2026-10-21 | **Handgepflegtes, das ein Paket liest, liegt unter `etc/`** an dem Pfad, den no-littering erwartet (`etc/yasnippet/snippets/`, `etc/eshell/aliases`), statt in eigenen Verzeichnissen im Wurzelverzeichnis wie im Zielbild ursprünglich gezeichnet. | Eine Konvention statt einer Ausnahme je Paket; das Wurzelverzeichnis bleibt bei `init.el`, `early-init.el` und den Quellverzeichnissen. |
| 2026-10-21 | **`exec-path-from-shell` liest den PATH**, vorerst. | Ersetzt den Eigenbau, der in der GUI unter macOS die Login-Shell nicht las. `emacs-plus` bringt einen eingebauten PATH mit; ob das genügt, entscheidet das Paket-Review. |
| 2026-10-21 | **Bekannte Fehler (Plan, B1–B7) werden im Review des jeweiligen Pakets behoben**, nicht vorgezogen. | Manche Pakete fallen dabei ganz weg; ein vorgezogener Fix wäre dann verlorene Arbeit. |
