# Tests ohne Netzwerk: verwenden den mitgelieferten Snapshot (inst/extdata)
snapshot_daten <- function() {
  ds <- ogd_datensaetze()
  roh <- lies_ogd_csv(app_sys("extdata", ds$beschaeftigte$snapshot))
  bez <- lies_ogd_csv(app_sys("extdata", ds$bezirke$snapshot))
  berechne_ebenen(roh, bez, lade_ch_anteile(), lade_noga_abschnitte())
}

test_that("Kennzahlen werden pro Ebene berechnet", {
  tab <- snapshot_daten()
  expect_named(tab, c("kanton", "c_sektor", "bezirk", "gemeinde"))
  expect_length(unique(tab$bezirk$gebiet), 5)
  expect_length(unique(tab$gemeinde$gebiet), 80)
  expect_false(anyNA(tab$kanton$branche))
  expect_false(anyNA(tab$c_sektor$branche))

  # Standortquotient: TG-Anteil / CH-Anteil; Kanton gewichtet = 1
  k <- tab$kanton[tab$kanton$jahr == max(tab$kanton$jahr), ]
  ch <- lade_ch_anteile()
  ch <- ch[ch$ebene == "abschnitt" & ch$jahr == max(k$ch_jahr), ]
  expect_equal(sum(k$standortquotient * ch$anteil_ch[match(k$code, ch$code)]), 1)
})

test_that("Wachstum vergleicht exakt zehn Jahre", {
  df <- data.frame(
    jahr = c(2010L, 2015L, 2020L), gemeinde = "A", bezirk = "B", abschnitt = "C",
    abteilung = "10", abteilung_text = "x", beschaeftigte = c(100, 150, 200)
  )
  ch <- data.frame(jahr = 2010L, ebene = "abschnitt", code = "C", anteil_ch = 0.5)
  res <- berechne_kennzahlen(df, "gemeinde", "abschnitt", ch)
  expect_equal(res$wachstum[res$jahr == 2020], (2^(1 / 10) - 1) * 100)
  expect_true(is.na(res$wachstum[res$jahr == 2015]))
  expect_equal(res$ch_jahr, rep(2010L, 3))
  expect_equal(res$standortquotient, rep(2, 3))
})

test_that("Referenzjahr fällt auf jüngstes verfügbares Jahr zurück", {
  expect_equal(referenzjahr(c(2022L, 2024L, 2025L), c(2022L, 2023L, 2024L)), c(2022L, 2024L, 2024L))
})

test_that("Diagrammdaten und Texte", {
  tab <- snapshot_daten()
  max_year <- max(tab$kanton$jahr)
  d <- lapply(tab, diagrammdaten, max_year = max_year, min_year = max_year - 10L)
  expect_named(d$kanton, "Thurgau")
  expect_named(d$kanton$Thurgau, c("name", "current", "previous", "x", "y", "z"))
  expect_length(d$gemeinde, 80)

  txt <- chart_text("dumbbell", "gemeinde", "Arbon", max_year - 10L, max_year)
  expect_match(txt$title, paste0("Gemeinde Arbon .*", max_year - 10L, "-", max_year))
  expect_match(chart_text("bar", "c_sektor", "Thurgau", 2014, 2024)$text, "grünen")

  html <- as.character(render_lesebeispiel(d$kanton$Thurgau, max_year, max_year - 10L))
  expect_match(html, "Gesundheits- und Sozialwesen")

  for (level in names(d)) {
    x <- d[[level]][[1]]
    expect_s3_class(create_bubble_chart(x, level, max_year, max_year - 10L, "Top 3"), "highchart")
    expect_s3_class(create_dumbbell_chart(x, max_year, max_year - 10L), "highchart")
    expect_s3_class(create_bar_chart(x, level, max_year), "highchart")
  }
})

test_that("Fallback auf Snapshot ohne Netz und Cache", {
  withr_dir <- tempfile()
  old <- options(branchenapp.cache_dir = withr_dir)
  on.exit(options(old))
  local_mocked_bindings(lade_datei = function(url, ziel) FALSE)
  expect_warning(roh <- lade_ogd_datensatz(ogd_datensaetze()$bezirke), "mitgelieferten")
  expect_equal(attr(roh, "quelle"), "snapshot")
})
