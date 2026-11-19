# Aufbereitung der OGD-Daten -------------------------------------------------

#' Ebenen der App (Reihenfolge = Module)
#' @noRd
ebenen <- function() {
  list(
    kanton = list(download = "Branchenstruktur_Kanton"),
    c_sektor = list(download = "Branchenstruktur_Verarbeitendes_Gewerbe"),
    bezirk = list(download = "Branchenstruktur_Bezirke"),
    gemeinde = list(download = "Branchenstruktur_Gemeinden")
  )
}

#' Schweizer Referenzanteile (Anteil Beschäftigte einer Branche an allen
#' Beschäftigten der Schweiz, BFS STATENT). Aktualisierung: `data-raw/`.
#' @noRd
lade_ch_anteile <- function() {
  ch <- utils::read.csv(app_sys("extdata", "beschaeftigte_ch_anteile.csv"),
                        colClasses = c(code = "character"))
  ch$jahr <- as.integer(ch$jahr)
  ch
}

#' Namen der NOGA-Abschnitte (Gross-/Kleinschreibung wie in der Publikation)
#' @noRd
lade_noga_abschnitte <- function() {
  utils::read.csv(app_sys("extdata", "noga_abschnitte.csv"), encoding = "UTF-8")
}

#' Rohdaten (sk-stat-150) bereinigen und Bezirke (sk-stat-79) ergänzen
#'
#' Gemeinden werden über die BFS-Nummer zugeordnet und mit den Namen aus
#' sk-stat-79 versehen, da die Namen in sk-stat-150 nicht über alle Jahre
#' einheitlich geschrieben sind.
#' @param roh,bezirke Rückgabe von [lade_ogd_datensatz()].
#' @noRd
bereinige_rohdaten <- function(roh, bezirke) {
  bez <- stats::setNames(bezirke$bezirk, bezirke$bfs_gemeindenummer)
  gem <- stats::setNames(bezirke$gemeinde, bezirke$bfs_gemeindenummer)
  name <- unname(gem[roh$bfs_nr_gemeinde])
  data.frame(
    jahr = as.integer(substr(roh$jahr, 1, 4)),
    gemeinde = ifelse(is.na(name), roh$gemeinde, name),
    bezirk = unname(bez[roh$bfs_nr_gemeinde]),
    abschnitt = roh$noga_section_code,
    abteilung = sprintf("%02d", as.integer(roh$noga_zweisteller_code)),
    abteilung_text = roh$noga_zweisteller_text,
    beschaeftigte = as.numeric(roh$beschaeftigte_total)
  )
}

#' Jahr der Schweizer Referenz je Jahr: gleiches Jahr, sonst jüngstes verfügbares
#' @param jahre Jahre der Thurgauer Daten.
#' @param ch_jahre Verfügbare Jahre der Schweizer Referenz.
#' @noRd
referenzjahr <- function(jahre, ch_jahre) {
  vapply(jahre, function(j) {
    kandidaten <- ch_jahre[ch_jahre <= j]
    if (length(kandidaten)) max(kandidaten) else min(ch_jahre)
  }, integer(1))
}

#' Kennzahlen einer Ebene berechnen
#'
#' @param df Bereinigte Rohdaten (ein Eintrag pro Gemeinde x Abteilung x Jahr).
#' @param gebiet Spalte, nach der gruppiert wird (`NULL` = ganzer Kanton).
#' @param branche Spalte mit dem Branchenschlüssel (`abschnitt`/`abteilung`).
#' @param ch Schweizer Referenzanteile der passenden Ebene (`code`, `jahr`, `anteil_ch`).
#' @return data.frame mit `gebiet`, `jahr`, `code`, `beschaeftigte`,
#'   `standortquotient`, `wachstum` (mittleres jährliches Wachstum über zehn
#'   Jahre in %).
#' @noRd
berechne_kennzahlen <- function(df, gebiet, branche, ch) {
  df$gebiet <- if (is.null(gebiet)) "Thurgau" else df[[gebiet]]
  df$code <- df[[branche]]
  df <- df[!is.na(df$gebiet), ]

  agg <- stats::aggregate(beschaeftigte ~ gebiet + jahr + code, data = df, FUN = sum)
  total <- stats::aggregate(beschaeftigte ~ gebiet + jahr, data = df, FUN = sum)
  names(total)[3] <- "total"
  agg <- merge(agg, total, by = c("gebiet", "jahr"))

  agg$ch_jahr <- referenzjahr(agg$jahr, unique(ch$jahr))
  agg <- merge(agg, ch[, c("jahr", "code", "anteil_ch")],
               by.x = c("ch_jahr", "code"), by.y = c("jahr", "code"), all.x = TRUE)
  agg$standortquotient <- (agg$beschaeftigte / agg$total) / agg$anteil_ch

  vor10 <- agg[, c("gebiet", "jahr", "code", "beschaeftigte")]
  names(vor10)[4] <- "beschaeftigte_vor10"
  vor10$jahr <- vor10$jahr + 10L
  agg <- merge(agg, vor10, by = c("gebiet", "jahr", "code"), all.x = TRUE)
  agg$wachstum <- ((agg$beschaeftigte / agg$beschaeftigte_vor10)^(1 / 10) - 1) * 100

  agg <- agg[order(agg$gebiet, agg$jahr, agg$code), ]
  rownames(agg) <- NULL
  agg[, c("gebiet", "jahr", "ch_jahr", "code", "beschaeftigte", "standortquotient", "wachstum")]
}

#' Alle Ebenen aus den Rohdaten berechnen
#'
#' @param roh,bezirke Rohdaten von data.tg.ch (sk-stat-150, sk-stat-79).
#' @param ch Schweizer Referenzanteile ([lade_ch_anteile()]).
#' @param abschnitte Namen der NOGA-Abschnitte ([lade_noga_abschnitte()]).
#' @return Benannte Liste (eine Tabelle pro Ebene) mit Spalte `branche`.
#' @noRd
berechne_ebenen <- function(roh, bezirke, ch, abschnitte) {
  df <- bereinige_rohdaten(roh, bezirke)
  ch_abschnitt <- ch[ch$ebene == "abschnitt", ]
  ch_abteilung <- ch[ch$ebene == "abteilung", ]

  res <- list(
    kanton = berechne_kennzahlen(df, NULL, "abschnitt", ch_abschnitt),
    c_sektor = berechne_kennzahlen(df, NULL, "abteilung", ch_abteilung),
    bezirk = berechne_kennzahlen(df, "bezirk", "abschnitt", ch_abschnitt),
    gemeinde = berechne_kennzahlen(df, "gemeinde", "abschnitt", ch_abschnitt)
  )

  # Verarbeitendes Gewerbe: nur Abteilungen des Abschnitts C
  abt_c <- unique(df[df$abschnitt == "C", c("abteilung", "abteilung_text")])
  abt_c <- abt_c[!duplicated(abt_c$abteilung), ]
  res$c_sektor <- res$c_sektor[res$c_sektor$code %in% abt_c$abteilung, ]

  namen_abschnitt <- stats::setNames(abschnitte$branche, abschnitte$code)
  namen_abteilung <- stats::setNames(abt_c$abteilung_text, abt_c$abteilung)
  lapply(stats::setNames(names(res), names(res)), function(ebene) {
    d <- res[[ebene]]
    namen <- if (ebene == "c_sektor") namen_abteilung else namen_abschnitt
    d$branche <- unname(namen[d$code])
    d
  })
}

#' Diagrammdaten: letztes Jahr gegenüber zehn Jahre davor
#'
#' @param df Tabelle einer Ebene aus [berechne_ebenen()].
#' @param max_year,min_year Vergleichsjahre.
#' @return Benannte Liste (ein data.frame pro Gebiet) mit `name`, `current`,
#'   `previous`, `x` (Standortquotient), `y` (Wachstum), `z` (Beschäftigte).
#' @noRd
diagrammdaten <- function(df, max_year, min_year) {
  akt <- df[df$jahr == max_year, ]
  vor <- df[df$jahr == min_year, c("gebiet", "code", "beschaeftigte")]
  names(vor)[3] <- "previous"
  d <- merge(akt, vor, by = c("gebiet", "code"))
  d <- d[!is.na(d$beschaeftigte) & !is.na(d$previous) & !is.na(d$standortquotient) & !is.na(d$wachstum), ]
  d <- data.frame(
    gebiet = d$gebiet, name = d$branche, current = d$beschaeftigte, previous = d$previous,
    x = d$standortquotient, y = d$wachstum, z = d$beschaeftigte
  )
  lapply(split(d[, -1], d$gebiet), `rownames<-`, NULL)
}

#' Downloadtabelle einer Ebene
#' @noRd
downloadtabelle <- function(df, ebene) {
  out <- data.frame(
    Jahr = df$jahr,
    Gebiet = df$gebiet,
    `NOGA-Code` = df$code,
    Branche = df$branche,
    Beschäftigte = df$beschaeftigte,
    Standortquotient = round(df$standortquotient, 3),
    `Wachstum 10 Jahre (% p.a.)` = round(df$wachstum, 2),
    `Referenzjahr Schweiz` = df$ch_jahr,
    check.names = FALSE
  )
  if (ebene %in% c("kanton", "c_sektor")) out$Gebiet <- NULL
  out
}

# Cache, damit die Daten nur einmal pro R-Prozess (und danach höchstens
# einmal pro `ogd_cache_stunden()`) geladen und aufbereitet werden.
.cache <- new.env(parent = emptyenv())

#' Alle Daten der App laden und aufbereiten (gecacht)
#'
#' @param neu `TRUE` erzwingt ein Neuladen.
#' @return Liste mit `max_year`, `min_year`, `ch_jahr`, `stand`, `quelle`,
#'   `tabellen` (eine Tabelle pro Ebene, alle Jahre) und `daten` (Diagrammdaten).
#' @noRd
load_branchen_data <- function(neu = FALSE) {
  if (!neu && !is.null(.cache$data) &&
      difftime(Sys.time(), .cache$zeit, units = "hours") < ogd_cache_stunden()) {
    return(.cache$data)
  }

  ds <- ogd_datensaetze()
  roh <- lade_ogd_datensatz(ds$beschaeftigte)
  bezirke <- lade_ogd_datensatz(ds$bezirke)
  tabellen <- berechne_ebenen(roh, bezirke, lade_ch_anteile(), lade_noga_abschnitte())

  max_year <- max(tabellen$kanton$jahr)
  min_year <- max_year - 10L
  daten <- lapply(tabellen, diagrammdaten, max_year = max_year, min_year = min_year)

  .cache$zeit <- Sys.time()
  .cache$data <- list(
    max_year = max_year,
    min_year = min_year,
    ch_jahr = max(tabellen$kanton$ch_jahr[tabellen$kanton$jahr == max_year]),
    quelle = attr(roh, "quelle"),
    stand = attr(roh, "stand"),
    tabellen = tabellen,
    daten = daten
  )
  .cache$data
}
