test_that("Daten werden pro Gebiet aufbereitet", {
  app_data <- load_branchen_data()
  expect_equal(app_data$min_year, app_data$max_year - 10)
  expect_named(app_data$daten, c("kanton", "c_sektor", "bezirk", "gemeinde"))
  expect_named(app_data$daten$kanton, "Thurgau")
  expect_gt(length(app_data$daten$bezirk), 1)
  expect_gt(length(app_data$daten$gemeinde), 50)

  for (lvl in app_data$daten) for (d in lvl) {
    expect_named(d, c("name", "current", "previous", "x", "y", "z"))
    expect_false(anyNA(d$current) || anyNA(d$previous))
  }
})

test_that("Texte enthalten Gebiet und Zeitraum", {
  txt <- chart_text("dumbbell", "gemeinde", "Arbon", 2013, 2023)
  expect_match(txt$title, "in der Gemeinde Arbon")
  expect_match(txt$title, "2013-2023")
  expect_null(chart_text("bubble", "kanton", "Thurgau", 2013, 2023)$text)
  expect_match(chart_text("bar", "c_sektor", "Thurgau", 2013, 2023)$text, "grünen")
})

test_that("Lesebeispiel wird aus den Daten berechnet", {
  app_data <- load_branchen_data()
  html <- as.character(render_lesebeispiel(
    app_data$daten$kanton$Thurgau, app_data$max_year, app_data$min_year
  ))
  expect_match(html, "Gesundheits- und Sozialwesen")
  expect_match(html, as.character(app_data$max_year))
})

test_that("Diagramme werden erstellt", {
  skip_if_not_installed("highcharter")
  app_data <- load_branchen_data()
  for (level in names(app_data$daten)) {
    d <- app_data$daten[[level]][[1]]
    expect_s3_class(create_bubble_chart(d, level, app_data$max_year, app_data$min_year, "Top 3"), "highchart")
    expect_s3_class(create_dumbbell_chart(d, app_data$max_year, app_data$min_year), "highchart")
    expect_s3_class(create_bar_chart(d, level, app_data$max_year), "highchart")
  }
})
