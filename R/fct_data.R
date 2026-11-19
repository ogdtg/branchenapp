#' Konfiguration der Gebietsebenen
#'
#' Pro Ebene: Quelldatei, Gruppierungsspalte (Bezirk/Gemeinde), Branchen-
#' schlüssel und -name, Standortquotient, Wachstum sowie Download-Datei.
#' @noRd
ebenen <- list(
  kanton = list(
    rds = "daten_kanton_use.rds", xlsx = "daten_kanton.xlsx",
    download = "Kanton_Wirtschaftsstruktur",
    group = NULL, key = "noga_section_code", name = "noga_section_text_lower",
    sq = "Standortquotient_tg", growth = "zehn_jahr_wachstum_tg"
  ),
  c_sektor = list(
    rds = "daten_sectorC_use.rds", xlsx = "daten_sectorC.xlsx",
    download = "Verarbeitendes_Gewerbe_Struktur",
    group = NULL, key = "noga_zweisteller_code", name = "noga_zweisteller_text",
    sq = "Standortquotient_tg_c", growth = "zehn_jahr_wachstum_tg"
  ),
  bezirk = list(
    rds = "daten_bezirk_use.rds", xlsx = "daten_bezirk.xlsx",
    download = "Bezirk_Wirtschaftsstruktur",
    group = "bezirk", key = "noga_section_code", name = "noga_section_text_lower",
    sq = "Standortquotient_bezirk", growth = "zehn_jahr_wachstum_bezirk"
  ),
  gemeinde = list(
    rds = "daten_gemeinde_use.rds", xlsx = "daten_gemeinde.xlsx",
    download = "Gemeinde_Wirtschaftsstruktur",
    group = "gemeinde", key = "noga_section_code", name = "noga_section_text_lower",
    sq = "Standortquotient_gemeinde", growth = "zehn_jahr_wachstum_gemeinde"
  )
)

#' Daten für die Diagramme aufbereiten
#'
#' Vergleicht das letzte Jahr mit dem Jahr zehn Jahre davor und liefert pro
#' Gebiet einen Datensatz mit den Spalten `name`, `current`, `previous`,
#' `x` (Standortquotient), `y` (Wachstum) und `z` (Beschäftigte).
#'
#' @param df Rohdaten einer Ebene (siehe `inst/extdata/*_use.rds`).
#' @param cfg Eintrag aus `ebenen`.
#' @param max_year,min_year Vergleichsjahre.
#' @return Benannte Liste von data.frames (ein Element pro Gebiet).
#' @noRd
prepare_chart_data <- function(df, cfg, max_year, min_year) {
  group <- if (is.null(cfg$group)) rep("Thurgau", nrow(df)) else df[[cfg$group]]
  df <- data.frame(
    gebiet = group,
    jahr = df$jahr,
    key = df[[cfg$key]],
    name = df[[cfg$name]],
    beschaeftigte = df$beschaeftigte_total,
    x = df[[cfg$sq]],
    y = df[[cfg$growth]]
  )
  df <- df[!is.na(df$gebiet), ]

  current <- df |> filter(jahr == max_year)
  previous <- df |>
    filter(jahr == min_year) |>
    select("gebiet", "key", previous = "beschaeftigte")

  res <- current |>
    inner_join(previous, by = c("gebiet", "key")) |>
    transmute(
      gebiet, name,
      current = beschaeftigte,
      previous,
      x, y,
      z = beschaeftigte
    ) |>
    filter(!is.na(current), !is.na(previous))

  lapply(split(res[, -1], res$gebiet), function(d) {
    rownames(d) <- NULL
    d
  })
}

# Cache, damit die Daten nur einmal pro R-Prozess aufbereitet werden
.cache <- new.env(parent = emptyenv())

#' Alle Daten der App laden
#'
#' @return Liste mit `max_year`, `min_year` und pro Ebene einer benannten
#'   Liste von Diagramm-Datensätzen.
#' @noRd
load_branchen_data <- function() {
  if (!is.null(.cache$data)) return(.cache$data)

  roh <- lapply(ebenen, function(cfg) readRDS(app_sys("extdata", cfg$rds)))
  max_year <- max(roh$kanton$jahr, na.rm = TRUE)
  min_year <- max_year - 10

  daten <- Map(
    function(df, cfg) prepare_chart_data(df, cfg, max_year, min_year),
    roh, ebenen
  )

  .cache$data <- list(max_year = max_year, min_year = min_year, daten = daten)
  .cache$data
}
