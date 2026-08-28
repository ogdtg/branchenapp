# Branchenapp

A Shiny dashboard showing the economic/employment structure ("Branchenstruktur")
of the canton of Thurgau (Switzerland), published by the Amt für Daten und
Statistik Thurgau.

## What it does

The app visualizes the number of employees per economic sector (NOGA
sections), based on open government data (OGD) from the Bundesamt für
Statistik (STATENT). For each sector it shows:

- how many people are employed in it (size),
- how it has grown over the last ten years (growth), and
- how strongly it is represented compared to Switzerland as a whole
  (Standortquotient / location quotient).

Data is available at three geographic levels, each its own tab:

- **Kanton** – the canton of Thurgau as a whole, plus a dedicated zoom on the
  "Verarbeitendes Gewerbe" (manufacturing, NOGA section C), the largest
  sector in the canton.
- **Bezirke** – the districts of Thurgau, selectable via a dropdown.
- **Gemeinden** – the municipalities of Thurgau, selectable via a dropdown.

Each level offers three chart views (bubble/overview, dumbbell detail on
size & growth, and a bar chart on the location quotient), plus buttons to
download the underlying data as Excel, open the OGD dataset, and links to
related articles, statistics and a glossary.

## Project structure

```
app.R                    Shiny UI and server logic
R/
  01_load_packages.R      packages required to run the app
  02_init_ui.R            dashboard header/body scaffolding + Lesebeispiel text
  03_tab2.R               bar chart (location quotient)
  04_tab1.R               dumbbell chart (size & growth over 10 years)
  05_tab3.R               bubble chart (overview)
Daten/
  02_b_data.R             one-off script to (re-)build the data below whenever
                          the OGD source data is updated
  *_use.rds               fast-loading data used by the app itself
  daten_*.xlsx            formatted workbooks offered for download in the app
  match_zweisteller_sektion.xlsx  mapping table used while building the data
www/
  dashboard_style.css      styling
```

## Updating the data

Whenever the underlying OGD datasets are refreshed, run `Daten/02_b_data.R`
once. It pulls the latest data via the `BFS` and `tgAPI` packages,
recalculates the location quotients and 10-year growth rates, and writes:

- `Daten/*_use.rds` — the data the app reads at startup, and
- `Daten/*.xlsx` — the formatted workbooks users can download from the app.

The app itself only reads the `*_use.rds` files and never needs the data-prep
packages (`BFS`, `tgAPI`, `openxlsx`, `TGexcel`, `readxl`, `stringr`) at
runtime.

## Running the app

Open `app.R` in RStudio and click "Run App", or run:

```r
shiny::runApp()
```
