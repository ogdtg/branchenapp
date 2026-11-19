# Aktualisiert die mitgelieferten Referenzdaten der App (aus dem Paket-Root
# ausführen). Die Thurgauer Daten lädt die App selbst live von data.tg.ch;
# nötig ist dieses Skript nur
#  1. wenn das BFS ein neues STATENT-Jahr publiziert (Schweizer Referenz für
#     den Standortquotienten) und
#  2. um den Offline-Snapshot (Fallback, falls data.tg.ch nicht erreichbar)
#     aufzufrischen.
#
# Zusätzlich benötigte Pakete: BFS, readxl, stringr

library(dplyr)

# 1. Schweizer Branchenanteile (BFS STATENT) ---------------------------------
ch <- BFS::bfs_get_data(number_bfs = "px-x-0602010000_101", language = "de") |>
  filter(
    Kanton == "Schweiz",
    Beobachtungseinheit == "Beschäftigte",
    Wirtschaftsabteilung != "Wirtschaftsabteilung - Total"
  ) |>
  transmute(
    jahr = as.integer(Jahr),
    abteilung = stringr::str_extract(Wirtschaftsabteilung, "^\\d{2}"),
    beschaeftigte = `Arbeitsstätten und Beschäftigte`
  )

zuordnung <- readxl::read_xlsx("data-raw/match_zweisteller_sektion.xlsx") |>
  transmute(
    abteilung = sprintf("%02d", as.integer(noga_zweisteller_code)),
    abschnitt = noga_section_code,
    branche = noga_section_text_lower
  )

ch <- ch |>
  left_join(zuordnung, by = "abteilung") |>
  group_by(jahr) |>
  mutate(anteil = beschaeftigte / sum(beschaeftigte, na.rm = TRUE)) |>
  ungroup()

anteile <- bind_rows(
  ch |>
    filter(!is.na(abschnitt)) |>
    group_by(jahr, code = abschnitt) |>
    summarise(anteil_ch = sum(anteil, na.rm = TRUE), .groups = "drop") |>
    mutate(ebene = "abschnitt"),
  ch |>
    filter(abschnitt == "C") |>
    transmute(jahr, code = abteilung, anteil_ch = anteil, ebene = "abteilung")
) |>
  select(jahr, ebene, code, anteil_ch) |>
  arrange(ebene, code, jahr)

write.csv(anteile, "inst/extdata/beschaeftigte_ch_anteile.csv", row.names = FALSE)

# Namen der NOGA-Abschnitte
zuordnung |>
  distinct(code = abschnitt, branche) |>
  filter(!is.na(code)) |>
  arrange(code) |>
  write.csv("inst/extdata/noga_abschnitte.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 2. Offline-Snapshot der OGD-Daten ------------------------------------------
pkgload::load_all(".")
for (ds in ogd_datensaetze()) {
  tmp <- tempfile(fileext = ".csv")
  stopifnot(lade_datei(ogd_csv_url(ds$id, ds$select), tmp))
  ziel <- file.path("inst/extdata", ds$snapshot)
  if (grepl("\\.gz$", ziel)) {
    writeLines(readLines(tmp, encoding = "UTF-8"), gz <- gzfile(ziel, "w", encoding = "UTF-8"))
    close(gz)
  } else {
    file.copy(tmp, ziel, overwrite = TRUE)
  }
}
