# branchenapp

Shiny-Dashboard zur **Branchenstruktur des Kantons Thurgau** (Amt für Daten
und Statistik). Gebaut als [golem](https://thinkr-open.github.io/golem/)-App
im Thurgauer Designstil von [`tgdashboard`](https://github.com/ogdtg/tgdashboard).

Pro NOGA-Branche zeigt die App auf Ebene **Kanton** (inkl. Zoom auf das
verarbeitende Gewerbe), **Bezirke** und **Gemeinden**:

- **Übersicht** (Bubble): Grösse, Wachstum und Standortquotient
- **Grösse und Wachstum** (Dumbbell): Beschäftigte vor zehn Jahren vs. heute
- **Standortquotient** (Balken): Vertretung im Vergleich zur Schweiz

In der Fusszeile jeder Karte: Datenquelle, Excel-Download, weiterführende Links.

## Daten

Die App lädt beim Start direkt von [data.tg.ch](https://data.tg.ch):

- [`sk-stat-150`](https://data.tg.ch/explore/dataset/sk-stat-150/) Beschäftigte nach Wirtschaftszweigen und Gemeinden
- [`sk-stat-79`](https://data.tg.ch/explore/dataset/sk-stat-79/) Gemeinden nach Bezirken

Standortquotient und Zehnjahreswachstum werden daraus berechnet; die Jahre
(aktuellstes Jahr und zehn Jahre davor) ergeben sich automatisch aus den Daten.
Die Schweizer Referenz (BFS STATENT) liegt in
`inst/extdata/beschaeftigte_ch_anteile.csv`. Fehlt dort ein neues Jahr, wird
das jüngste vorhandene verwendet und in der App darauf hingewiesen.

Caching: Die aufbereiteten Daten bleiben pro R-Prozess im Speicher, die
Downloads werden als Datei gecacht (Standard 24 h). Ist data.tg.ch nicht
erreichbar, wird der Cache und zuletzt der mitgelieferte Snapshot verwendet.

| Option / Umgebungsvariable | Bedeutung | Standard |
|---|---|---|
| `branchenapp.cache_dir` / `BRANCHENAPP_CACHE_DIR` | Cache-Verzeichnis | `tools::R_user_dir("branchenapp", "cache")` |
| `branchenapp.cache_stunden` | Maximales Alter der Daten | 24 |
| `branchenapp.timeout` | Download-Timeout (Sekunden) | 30 |

## Installation & Start

```r
remotes::install_github("ogdtg/tgdashboard")
remotes::install_local(".")   # oder devtools::load_all()
branchenapp::run_app()
```

Entwicklung: `dev/run_dev.R`. Deployment auf Posit Connect über `app.R`;
vorher `rsconnect::writeManifest()` ausführen. Empfohlen auf Connect:
*Min processes* = 1 (Daten bleiben warm geladen).

## Struktur

```
R/
  app_ui.R          Seite: tg_header, bslib::page_fixed, Ebenen-Tabs, tg_footer
  app_server.R      startet ein Modul pro Ebene
  mod_branchen.R    Diagramm-Karte (navset_card_tab) inkl. Gebietsauswahl/Download
  fct_charts.R      Highcharts: Bubble, Dumbbell, Balken
  fct_ogd.R         Download von data.tg.ch mit Cache und Fallback
  fct_data.R        Berechnung Standortquotient/Wachstum, Daten-Cache
  fct_quellen.R     Quellen und Links in den Karten, Ladeanzeige
  utils_text.R      Titel, Beschreibungen, Lesebeispiel
  app_config.R, run_app.R   golem-Standard
inst/
  extdata/          Schweizer Referenz, NOGA-Namen, Offline-Snapshot
  app/www/          favicon.ico, custom.css
  golem-config.yml
data-raw/
  referenzdaten.R   aktualisiert Schweizer Referenz und Snapshot
```

Branding (Farben, Schrift, Header, Footer) kommt direkt aus `tgdashboard`
(`_brand.yml`), Anpassungen in `inst/app/www/custom.css`.

## Referenzdaten aktualisieren

Bei einem neuen STATENT-Jahr `data-raw/referenzdaten.R` aus dem Paket-Root
ausführen (benötigt zusätzlich `BFS`, `readxl`, `stringr`). Die Thurgauer
Daten selbst aktualisieren sich automatisch.
