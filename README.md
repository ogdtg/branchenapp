# branchenapp

Shiny-Dashboard zur **Branchenstruktur des Kantons Thurgau** (Amt für Daten
und Statistik). Gebaut als [golem](https://thinkr-open.github.io/golem/)-App
im Thurgauer Designstil von [`tgdashboard`](https://github.com/ogdtg/tgdashboard).

Pro NOGA-Branche zeigt die App auf Ebene **Kanton** (inkl. Zoom auf das
verarbeitende Gewerbe), **Bezirke** und **Gemeinden**:

- **Übersicht** (Bubble): Grösse, Wachstum und Standortquotient
- **Grösse und Wachstum** (Dumbbell): Beschäftigte vor zehn Jahren vs. heute
- **Standortquotient** (Balken): Vertretung im Vergleich zur Schweiz
- **Daten und Quellen**: Excel-Download und Links

Datenquelle: BFS, STATENT (via OGD Thurgau `sk-stat-150`).

## Installation & Start

```r
remotes::install_github("ogdtg/tgdashboard")
remotes::install_local(".")   # oder devtools::load_all()
branchenapp::run_app()
```

Entwicklung: `dev/run_dev.R`. Deployment (Posit Connect / shinyapps.io) über
`app.R`; vorher `rsconnect::writeManifest()` ausführen.

## Struktur

```
R/
  app_ui.R          Seite: tg_header, bslib::page_fixed, Ebenen-Tabs, tg_footer
  app_server.R      startet ein Modul pro Ebene
  mod_branchen.R    Diagramm-Karte (navset_card_tab) inkl. Gebietsauswahl/Download
  fct_charts.R      Highcharts: Bubble, Dumbbell, Balken
  fct_data.R        Ebenen-Konfiguration, Laden und Aufbereiten der Daten
  utils_text.R      Titel, Beschreibungen, Lesebeispiel
  app_config.R, run_app.R   golem-Standard
inst/
  extdata/          *_use.rds (App-Daten), *.xlsx (Downloads)
  app/www/          favicon.ico, custom.css
  golem-config.yml
data-raw/
  branchen_daten.R  baut inst/extdata neu auf (bei neuen OGD-Daten ausführen)
```

Branding (Farben, Schrift, Header, Footer) kommt direkt aus `tgdashboard`
(`_brand.yml`), Anpassungen in `inst/app/www/custom.css`.

## Daten aktualisieren

`data-raw/branchen_daten.R` aus dem Paket-Root ausführen. Benötigt zusätzlich
`BFS`, `tgAPI`, `openxlsx`, `TGexcel`, `readxl`, `stringr` – diese werden
von der App selbst nicht gebraucht.
