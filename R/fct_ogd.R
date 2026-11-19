# Anbindung an data.tg.ch (OGD) ------------------------------------------------

#' OGD-Datensätze der App
#'
#' `select`: Nur benötigte Spalten laden (halbiert die Downloadgrösse).
#' `snapshot`: Mitgelieferte Kopie in `inst/extdata`, falls data.tg.ch nicht
#' erreichbar ist und kein Cache existiert.
#' @noRd
ogd_datensaetze <- function() {
  list(
    beschaeftigte = list(
      id = "sk-stat-150",
      select = c("gemeinde", "bfs_nr_gemeinde", "jahr", "noga_zweisteller_code",
                 "noga_zweisteller_text", "noga_section_code", "beschaeftigte_total"),
      snapshot = "sk-stat-150.csv.gz"
    ),
    bezirke = list(
      id = "sk-stat-79",
      select = c("bezirk", "gemeinde", "bfs_gemeindenummer"),
      snapshot = "sk-stat-79.csv"
    )
  )
}

#' Export-URL eines Datensatzes (CSV, Semikolon-getrennt)
#' @param id Datensatz-ID auf data.tg.ch.
#' @param select Optional: zu ladende Spalten.
#' @noRd
ogd_csv_url <- function(id, select = NULL) {
  url <- paste0("https://data.tg.ch/api/explore/v2.1/catalog/datasets/", id, "/exports/csv")
  if (length(select)) url <- paste0(url, "?select=", paste(select, collapse = "%2C"))
  url
}

#' Link auf die Datensatz-Seite
#' @param id Datensatz-ID auf data.tg.ch.
#' @noRd
ogd_seite_url <- function(id) {
  paste0("https://data.tg.ch/explore/dataset/", id, "/information/")
}

#' Cache-Verzeichnis für OGD-Downloads
#'
#' Option `branchenapp.cache_dir`, sonst Umgebungsvariable
#' `BRANCHENAPP_CACHE_DIR`, sonst Benutzer-Cache ([tools::R_user_dir()]).
#' @noRd
ogd_cache_dir <- function() {
  getOption(
    "branchenapp.cache_dir",
    Sys.getenv("BRANCHENAPP_CACHE_DIR", tools::R_user_dir("branchenapp", which = "cache"))
  )
}

#' Maximales Alter der Daten in Stunden (Option `branchenapp.cache_stunden`)
#' @noRd
ogd_cache_stunden <- function() {
  getOption("branchenapp.cache_stunden", 24)
}

#' Ist die Datei vorhanden und jünger als `max_stunden`?
#' @noRd
datei_ist_aktuell <- function(datei, max_stunden) {
  file.exists(datei) &&
    difftime(Sys.time(), file.mtime(datei), units = "hours") < max_stunden
}

#' OGD-CSV einlesen (Semikolon, UTF-8 mit BOM, auch .gz)
#' @param pfad Lokaler Pfad.
#' @noRd
lies_ogd_csv <- function(pfad) {
  con <- if (grepl("\\.gz$", pfad)) gzfile(pfad, encoding = "UTF-8-BOM") else file(pfad, encoding = "UTF-8-BOM")
  utils::read.csv(con, sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE,
                  colClasses = "character", check.names = FALSE)
}

#' Datei herunterladen (mit einem zweiten Versuch); `TRUE` bei Erfolg
#' @param url Quelle.
#' @param ziel Zieldatei.
#' @noRd
lade_datei <- function(url, ziel) {
  alt <- options(timeout = getOption("branchenapp.timeout", 30))
  on.exit(options(alt), add = TRUE)
  for (versuch in 1:2) {
    ok <- tryCatch(
      utils::download.file(url, ziel, mode = "wb", quiet = TRUE) == 0,
      error = function(e) FALSE,
      warning = function(w) FALSE
    )
    if (isTRUE(ok) && file.exists(ziel) && file.size(ziel) > 0) return(TRUE)
  }
  FALSE
}

#' OGD-Datensatz laden: Cache -> data.tg.ch -> alter Cache -> Snapshot
#'
#' @param ds Eintrag aus [ogd_datensaetze()].
#' @param cache_stunden Maximales Alter des Caches (0 = immer neu laden).
#' @return data.frame (alle Spalten als Text) mit Attribut `quelle`
#'   (`"data.tg.ch"`, `"cache"` oder `"snapshot"`) und `stand` (Zeitpunkt).
#' @noRd
lade_ogd_datensatz <- function(ds, cache_stunden = ogd_cache_stunden()) {
  cache <- file.path(ogd_cache_dir(), paste0(ds$id, ".csv"))

  if (datei_ist_aktuell(cache, cache_stunden)) {
    return(mit_herkunft(lies_ogd_csv(cache), "cache", file.mtime(cache)))
  }

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  if (lade_datei(ogd_csv_url(ds$id, ds$select), tmp)) {
    daten <- lies_ogd_csv(tmp)
    if (all(ds$select %in% names(daten))) {
      schreibe_cache(tmp, cache)
      return(mit_herkunft(daten, "data.tg.ch", Sys.time()))
    }
  }

  if (file.exists(cache)) {
    warning("data.tg.ch nicht erreichbar, verwende Cache vom ", format(file.mtime(cache)), " (", ds$id, ").")
    return(mit_herkunft(lies_ogd_csv(cache), "cache", file.mtime(cache)))
  }
  warning("data.tg.ch nicht erreichbar, verwende mitgelieferten Datenstand (", ds$id, ").")
  snapshot <- app_sys("extdata", ds$snapshot)
  mit_herkunft(lies_ogd_csv(snapshot), "snapshot", file.mtime(snapshot))
}

#' @noRd
mit_herkunft <- function(daten, quelle, stand) {
  attr(daten, "quelle") <- quelle
  attr(daten, "stand") <- stand
  daten
}

#' Heruntergeladene Datei in den Cache kopieren (Fehler werden ignoriert)
#' @noRd
schreibe_cache <- function(quelle, ziel) {
  try({
    dir.create(dirname(ziel), recursive = TRUE, showWarnings = FALSE)
    file.copy(quelle, ziel, overwrite = TRUE)
  }, silent = TRUE)
  invisible(ziel)
}
