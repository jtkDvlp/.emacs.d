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

Phase 1 des Plans ist umgesetzt: die Struktur steht. Phase 2 prüft die
Pakete Block für Block; erledigt sind 2.1 (Vervollständigung), 2.2
(Tastatur-Infrastruktur), 2.3 (Projekte) und 2.4 (Fenster und Tabs).

| Pfad | Inhalt | versioniert |
|---|---|---|
| `early-init.el` | package.el aus, native Kompilate nach `var/`, Frame ohne Leisten und maximiert. | ja |
| `init.el` | Nimmt `lisp/` und `modules/` in den `load-path` und lädt die Module in fester Reihenfolge — sonst nichts. | ja |
| `modules/init-packages.el` | elpaca-Installer (gepinnt über die Lockdatei), `elpaca-use-package`, no-littering, diminish. | ja |
| `modules/init-core.el` | Grundverhalten ohne eigenes Thema: `local.el`, PATH, Sicherungen, Kodierung, Auto-Revert, Scrollen, Rückfragen. | ja |
| `modules/init-<thema>.el` | Ein Thema je Datei: `keys`, `appearance`, `minibuffer`, `windows`, `editing`, `diff`, `project`, `treemacs`, `bookmarks`, `vc`, `org`, `startup`, `structural-editing`, `completion`, `checking`, `snippets`, `restclient`, `sql`, `httpd`, `logview`, `shell`. Jede Datei sagt im Kopf, was hineingehört. | ja |
| `modules/init-lang-<sprache>.el` | Je Sprache bzw. Dateiformat eine Datei. `init-lang-php` wird nicht geladen. | ja |
| `lisp/jtk-*.el` | **Eigene Bibliotheken** ohne Seiteneffekte beim Laden (`narrow`, `project`, `zoom`), Symbole mit Präfix `jtk-`. | ja |
| `etc/elpaca-lock.eld` | Commit jedes Pakets (88, mit Abhängigkeiten und elpaca selbst). | ja |
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
  Pakete landen dann in `~/emacs-next/elpaca/` — der erste Start dauert
  einige Minuten.
- Zu jedem Schritt gehört eine kurze Sichtungsliste: welche Tasten, welche
  Modi, welche Datei man öffnen soll, und was dabei passieren muss.

**`scripts/check` lädt die Konfiguration im Batch-Modus und
byte-kompiliert alle Dateien.** Der Bericht (Markdown) sagt, ob das Laden
klappte, und zählt je Datei Fehler und Warnungen; bei einem Ladefehler
kommt der Backtrace dazu. Es endet mit einem Fehler, wenn das Laden oder
eine Datei scheitert — Warnungen allein nicht, ihre Zahl ist das Maß. Die
GitHub-Action (`.github/workflows/check.yml`, macOS, Emacs 31.1) führt es
bei jedem Push aus und legt den Bericht in die Zusammenfassung des Laufs.
Gegen hängende Läufe: `scripts/check` gibt nach 8 Minuten auf und nennt
im Bericht die Pakete, auf die es noch wartete, mit ihrer letzten
Meldung und dem laufenden Prozess (sonst dauert ein Lauf 2,5 bis 3,5
Minuten). In der Action scheitert dazu ein Download unter 1 KB/s für
60 s, git fragt nie nach einem Passwort, und der Schritt hat ein
Zeitlimit von 15 Minuten. Mehrere Läufe hingen vorher stundenlang beim
Installieren der Pakete; die Ursache ist noch offen.
Ein anderes Emacs wählt man über `EMACS=/pfad/zu/emacs scripts/check`.

`--snapshot FILE` schreibt nach dem Laden — und nachdem jedes deklarierte
Paket zusätzlich geladen wurde — jede Tastenbelegung, Option, Hook-Funktion
und Dateizuordnung als sortierte Zeile in eine Datei (`scripts/snapshot.el`,
rund 17 700 Zeilen). Zwei Läufe ohne Änderung ergeben dieselbe Datei; der
Diff zweier Stände zeigt also genau, was sich am Verhalten geändert hat.
Ein reiner Umbau hat einen leeren Diff. Nicht erfasst: Faces, Modeline,
Fensterlayout — alles, was nur eine laufende GUI zeigt.

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
  dafür gibt es `:ensure (:wait t)` (`diminish`, `elpaca-use-package`,
  `no-littering`). Umgekehrt läuft ein Block mit
  `:ensure nil` **sofort**, auch wenn er weiter unten steht als ein
  Block, auf den er sich verlässt.
- **Eingebaute und lokale Pakete brauchen `:ensure nil`.** Sonst sucht
  elpaca ein Rezept (`jtk-*`, `sql`) oder holt eine zweite Fassung aus
  Git (`org`).
- **Was mit Emacs ausgeliefert wird, nie über elpaca beziehen.** GNU ELPA
  baut solche Pakete aus dem Emacs-Repository, und elpaca klont dann das
  ganze Repository (bei `which-key` 1,8 GB für eine Datei). Auf dem Mac hing
  der erste Start daran; in der Cloud und in CI fiel es nicht auf, weil
  die Leitung schnell genug war. `scripts/check` scheitert seitdem an
  jedem solchen Paket.
- **no-littering muss vor allen Paketen kommen, deren Pfade es setzt**, und
  es setzt nur Vorgaben: Ein späteres `setq` gewinnt. So bleiben
  `eshell-rc-script` (`~/.eshellrc`) und `mc/list-file` (`etc/`) bewusst
  abweichend.
- **Nach einem Paket-Update die Lockdatei fortschreiben**, sonst
  installiert ein neuer Rechner die alten Commits — und CI meldet die
  Abweichung erst, wenn sie gegen die Lockdatei prüft.
- **Strukturelles Editieren hat eine Belegung für alle Sprachen**
  (`jtk-structural-editing-keys` in `init-structural-editing`). Eine
  Sprache ruft `jtk-structural-editing-bind` mit ihrer Mode-Map und nennt
  nur ihre Abweichungen; wer dort Tasten wieder einzeln in `:bind`
  schreibt, hat zwei Stellen, die auseinanderlaufen.
- **Mit `lexical-binding` erreicht ein `let` die Variable eines Pakets nur,
  wenn das Paket schon geladen ist.** Sonst bindet es lexikalisch, und das
  Paket sieht den Wert nie. Vorher `require`n (so in `sql-mysql-for`) —
  weder Byte-Compiler noch Schnappschuss bemerken den Fehler.
- **Der Schnappschuss sieht nur, was geladen wird — und nur den
  Endzustand.** Ein Block, der an einem nie geladenen Paket hängt, fehlt
  vorher wie nachher (so `tide`, das auf das nicht installierte
  `typescript-mode` wartet). Und weil er alle Pakete auf einmal lädt,
  sieht er nicht, in welcher Reihenfolge eine echte Sitzung sie lädt:
  Dass CIDER die `C-c M-j`-Familie überschrieb, sobald eine `.clj`-Datei
  offen war, zeigte erst eine Spur der `define-key`-Aufrufe. Für solche
  Stellen gezielt nachladen und vergleichen.
- **Pakete, die beim Laden selbst Tasten setzen, schlagen `:bind`.**
  CIDER legt `C-c M-j` in `clojure-mode-map` zweimal fest (Autoloads und
  cider.el); die eigene Belegung muss danach kommen, siehe
  `jtk-clojure-bind-project-keys`.
- **Globale Tasten mit `:bind`, nicht mit `:bind*`.** `:bind*` schreibt in
  `override-global-map` und schlägt damit jede Mode — so verdeckte Org
  einst mit `C--` das globale Verengen (Plan, B1) und die Override-Map
  logviews eigenes `C-- s`. Ausnahmen mit Begründung im Code: `C-#` und
  `C-:`. Bei zwei Modulen auf derselben Taste gewinnt das später geladene;
  wer die Reihenfolge in `init.el` ändert, prüft das mit dem
  Schnappschuss.
- **Jeder globale Präfix ist eine benannte Map seines Moduls**
  (`jtk-<thema>-map`; Tabelle in der README). Ein einfacher Präfix über
  `:bind (:prefix-map … :prefix …)`, einer mit Wiederholtasten als
  `defvar-keymap` mit `:prefix t` und `:repeat t` — use-package kann
  `:prefix-map` und `:repeat-map` nicht verbinden. Präfixe, die mehrere
  Module teilen (`M-!`, `M-+`), stehen in `init-keys`.
- **Mit `:demand t` läuft `:config` vor den `:bind`-Formen.** Was in
  `:config` eine per `:bind` gefüllte Map liest, sieht sie leer.
- **Unter einer globalen Taste, die ein Befehl ist, gibt es keinen
  Präfix.** `("M-' c" . …)` mit `:bind` scheitert mit „starts with
  non-prefix key“, solange `M-'` noch `abbrev-prefix-mark` ist; in der
  Override-Map fiel das nie auf. Erst den Präfix als Map binden.
- **Projektwurzel über `jtk-project-root`, nicht `project-root`.**
  `project-root` kürzt das Home-Verzeichnis zu `~`; wer die Wurzel mit
  `buffer-file-name` oder einem eshell-Pfad vergleicht, findet dann
  nichts. `jtk-project-root` liefert sie absolut.
- **Einträge in `project-vc-ignores` mit Schrägstrich am Ende.** Aus
  `"node_modules"` macht project.el das git-Muster `**/node_modules`, das
  den Inhalt des Ordners nicht trifft; erst `"node_modules/"` wird zu
  `**/node_modules/**`.
- **embark-Aktionen auf einem Projekt treffen das markierte Projekt nur
  über `jtk-project-act-in-target`** (in `embark-around-action-hooks`).
  Die Projektbefehle fragen `project-current` des aktuellen Buffers;
  ohne den Hook liefen sie still im aktuellen statt im markierten
  Projekt — kein Fehler, nur das falsche Ziel.
- **Projektarten teilen sich die Tasten unter `j`.** Bei gleicher Taste
  gewinnt die zuerst angemeldete Art (heute Clojure vor npm vor
  PlatformIO, durch die Reihenfolge in `init.el` und weil npm und
  PlatformIO sich erst anmelden, wenn elpaca ihre Blöcke auswertet).
  Wer eine Art hinzufügt, prüft die Tasten gegen die übrigen.
- **`:bind*` mit `:map` wirkt wie `:bind`.** Die Belegung landet in der
  genannten Mode-Map, nicht in der Override-Map. Das Sternchen dort trägt
  nichts und täuscht Absicht vor.
- **`:custom` erreicht nur Optionen (`defcustom`).** Eine einfache
  Variable (`defvar`, etwa `ztree-diff-filter-list`) behält still ihren
  Vorgabewert; dort `setq` in `:init`, also vor dem Laden.
- **`:requires` überspringt die ganze Deklaration**, wenn das Feature beim
  Auswerten noch nicht geladen ist — still, ohne Meldung. `:after` ist fast
  immer das Gemeinte.
- **eshell-Tasten gehen über `eshell-mode-map*`**, weil eshell seine
  Mode-Map früher erst beim Start des Modus angelegt hat. Ob das in der
  Zielversion noch nötig ist, ist zu prüfen, bevor es wegfällt.

## Messwerte

| Stand | Laden | Pakete | Compile-Fehler (Anzahl) | Compile-Warnungen (Anzahl) |
|---|---|---|---:|---:|
| `a116aa1` (vor dem Umbau), CI, Emacs 31.1 ohne GUI | scheitert an `(scroll-bar-mode -1)` | — | — | — |
| nach Phase 1, Schritt 1.3, Cloud, Emacs 31.1 ohne GUI | ok | 93 installiert, Lockdatei reproduziert | 0 | 52 |
| nach Phase 1, Schritt 1.3, CI (macOS), Emacs 31.1 ohne GUI | ok | 93 installiert, Lockdatei reproduziert | 0 | 49 |
| nach Phase 1 (ohne which-key), Cloud, Emacs 31.1 ohne GUI | ok | 92 installiert, Lockdatei reproduziert | 0 | 31 |
| nach 2.1, Cloud, Emacs 31.1 ohne GUI | ok | alle installiert, Lockdatei mit 94 Einträgen | 0 | 31 |
| nach 2.2, Cloud, Emacs 31.1 ohne GUI | ok | alle installiert, Lockdatei mit 93 Einträgen reproduziert | 0 | 25 |
| nach 2.2 mit avy und expreg, Cloud, Emacs 31.1 ohne GUI | ok | alle installiert, Lockdatei mit 92 Einträgen reproduziert | 0 | 25 |
| nach 2.3, Cloud, Emacs 31.1 ohne GUI | ok | alle installiert, Lockdatei mit 91 Einträgen reproduziert | 0 | 23 |
| nach 2.4, Cloud, Emacs 31.1 ohne GUI | ok | alle installiert, Lockdatei mit 88 Einträgen reproduziert | 0 | 20 |

Der Unterschied von 3 Warnungen bis 2.1: Die `ns-*`- und `mac-*`-Variablen
gibt es nur unter macOS; in der Cloud meldete der Byte-Compiler sie als
frei. Seit 2.2 steht das `setq` auf oberster Ebene statt in einem `when`,
und dort warnt er nicht — Cloud und CI sollten jetzt gleich zählen.

**Startzeit (GUI, Mac):** `emacs-init-time` 0,387 s (gemessen nach
Schritt 1.3, 2026-10-21).

WATCHOUT: Mit elpaca ist `emacs-init-time` nicht die Startzeit. Die Uhr
bleibt stehen, bevor `after-init-hook` läuft — und dort aktiviert und
konfiguriert elpaca alle Pakete. Die volle Zeit misst erst
`elpaca-after-init-hook`:

```elisp
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Startup with packages: %.3f s"
                     (float-time (time-subtract (current-time)
                                                before-init-time)))))
```

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
