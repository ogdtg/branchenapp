#' Zugriff auf Dateien im Paket (inst/)
#'
#' @param ... Pfadbestandteile, an [system.file()] weitergereicht.
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "branchenapp")
}

#' golem-Konfiguration lesen
#'
#' @param value Name des Werts in `inst/golem-config.yml`.
#' @param config Aktive Konfiguration (Umgebungsvariable `GOLEM_CONFIG_ACTIVE`
#'   bzw. `R_CONFIG_ACTIVE`).
#' @param use_parent Übergeordnete Verzeichnisse durchsuchen?
#' @param file Pfad zur Konfigurationsdatei.
#' @noRd
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE,
  file = app_sys("golem-config.yml")
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
