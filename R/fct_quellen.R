# Links, Quellenhinweise und Ladeanzeige ---------------------------------------

#' Weiterführende Links (erscheinen in jeder Karte)
#' @noRd
weitere_links <- function() {
  list(
    "Webartikel zum Thema" = "https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit/arbeit-und-erwerb/beschaeftigte.html/6019/l/de/",
    "Wirtschaftsstatistiken" = "https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit.html/4433",
    "Wirtschaftsbarometer" = "https://wirtschaftsbarometer.tg.ch/",
    "Glossar" = "https://statistik.tg.ch/glossar/glossar.html/6736",
    "GitHub" = "https://github.com/ogdtg/branchenapp"
  )
}

#' @noRd
url_bfs_statent <- function() {
  "https://www.bfs.admin.ch/bfs/de/home/statistiken/industrie-dienstleistungen/unternehmen-beschaeftigte/wirtschaftsstruktur-unternehmen.html"
}

#' Externer Link (öffnet in neuem Tab)
#' @param text Linktext.
#' @param url Ziel-URL.
#' @noRd
externer_link <- function(text, url) {
  tags$a(href = url, target = "_blank", rel = "noopener", text)
}

#' Links mit Trennpunkt verbinden
#' @param links Liste von Tags.
#' @noRd
mit_trenner <- function(links) {
  n <- length(links)
  out <- vector("list", 2 * n - 1)
  out[seq(1, by = 2, length.out = n)] <- links
  out[seq(2, by = 2, length.out = n - 1)] <- list(" · ")
  tagList(out)
}

#' Fusszeile einer Karte: Datenquelle, Download, weiterführende Links
#' @param ns Namespace-Funktion des Moduls.
#' @noRd
karten_quellen <- function(ns) {
  links <- weitere_links()
  tagList(
    tags$p(
      class = "small text-muted mb-1",
      icon("database"), " Datenquelle: ",
      externer_link("Beschäftigte nach Wirtschaftszweigen (OGD, data.tg.ch)",
                    ogd_seite_url(ogd_datensaetze()$beschaeftigte$id)),
      " · Referenz Schweiz: ", externer_link("BFS, STATENT", url_bfs_statent()),
      " · ", downloadLink(ns("download"), tagList(icon("download"), " Daten herunterladen (Excel)"))
    ),
    tags$p(
      class = "small text-muted mb-0",
      icon("link"), " Weitere Informationen: ",
      mit_trenner(Map(externer_link, names(links), links))
    ),
    uiOutput(ns("hinweis"))
  )
}

#' Hinweis, falls die Schweizer Referenz älter ist als die Thurgauer Daten
#' @param app_data Rückgabe von [load_branchen_data()].
#' @noRd
hinweis_referenzjahr <- function(app_data) {
  if (app_data$ch_jahr >= app_data$max_year) return(NULL)
  tags$p(
    class = "small text-muted mb-0 mt-1",
    icon("circle-info"), " Für ", app_data$max_year,
    " liegen noch keine Schweizer Vergleichszahlen vor. Der Standortquotient wird mit der ",
    "Schweizer Branchenstruktur ", app_data$ch_jahr, " berechnet."
  )
}

#' Ladeanzeige (Spinner) für Grafiken
#' @param ui Output-Element.
#' @noRd
mit_spinner <- function(ui) {
  shinycssloaders::withSpinner(ui, type = 8, color = "#1849C8", size = 0.7)
}
