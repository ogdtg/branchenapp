# Highcharts-Diagramme der Branchenapp ---------------------------------------

#' @noRd
sq_help_icon <- paste0(
  '<i class="fas fa-question-circle help-icon" title="Der Standortquotient (SQ) ',
  "zeigt, wie stark eine Branche in einer Region im Vergleich zum nationalen ",
  "Durchschnitt vertreten ist. SQ > 1: überdurchschnittlich, SQ = 1: gleich wie ",
  'der nationale Durchschnitt, SQ < 1: unterdurchschnittlich"></i>'
)

#' @noRd
hc_quelle <- function(hc, max_year) {
  hc_credits(
    hc,
    enabled = TRUE,
    text = paste0("Datenquelle: Bundesamt für Statistik, STATENT ", max_year - 10, "-", max_year)
  )
}

#' Farbschemata: Standard (blau/violett) und verarbeitendes Gewerbe (grün/orange)
#' @noRd
chart_colors <- function(level) {
  if (level == "c_sektor") {
    list(
      over = "#ADD9A1", under = "#F5A951",
      quadrant = c(tr = "#8BAF81", br = "#c9e7c0", tl = "#DC9848", bl = "#f8cfa7"),
      label = c(tr = "#8BAF81", br = "#c9e7c0", tl = "#DC9848", bl = "#f8cfa7")
    )
  } else {
    list(
      over = "#4981b4", under = "#9d5b99",
      quadrant = c(tr = "#1d6aa2", br = "#aacbec", tl = "#863d83", bl = "#cea9cc"),
      label = c(tr = "#1d6aa2", br = "#8ab2d9", tl = "#863d83", bl = "#c39ec1")
    )
  }
}

#' Bubble-Chart (Übersicht): Standortquotient x Wachstum, Grösse = Beschäftigte
#'
#' @param data Datensatz aus [prepare_chart_data()].
#' @param level Ebene (`kanton`, `c_sektor`, `bezirk`, `gemeinde`).
#' @param legend_text Legendentext für die drei grössten Branchen.
#' @noRd
create_bubble_chart <- function(data, level, max_year, min_year, legend_text) {
  cols <- chart_colors(level)

  # Achsenbereich mit etwas Rand
  x_pad <- if (level == "kanton") 0.1 else max(0.1, diff(range(data$x, na.rm = TRUE)) * 0.1)
  y_pad <- if (level == "kanton") 0.5 else 2
  minx <- min(data$x, na.rm = TRUE) - x_pad
  maxx <- max(data$x, na.rm = TRUE) + x_pad
  miny <- min(data$y, na.rm = TRUE) - y_pad
  maxy <- max(data$y, na.rm = TRUE) + y_pad

  data <- data |>
    mutate(
      color = ifelse(
        x > 1,
        ifelse(y > 0, cols$quadrant[["tr"]], cols$quadrant[["br"]]),
        ifelse(y > 0, cols$quadrant[["tl"]], cols$quadrant[["bl"]])
      ),
      x = round(x, 2),
      y = round(y, 2)
    ) |>
    arrange(desc(z)) |>
    mutate(is_top3 = seq_along(z) <= 3)

  punkte <- lapply(seq_len(nrow(data)), function(i) {
    d <- data[i, ]
    list(
      x = d$x, y = d$y, z = d$z, name = d$name, color = d$color,
      marker = if (d$is_top3) list(lineWidth = 1, lineColor = "#000000") else list(lineWidth = 1),
      dataLabels = list(
        enabled = d$is_top3,
        format = "{point.name}",
        style = list(color = "#000000", fontWeight = "bold", fontSize = "11px", textOutline = "2px white"),
        verticalAlign = "bottom", x = 0, y = 10
      )
    )
  })

  quadrant_label <- function(hc, x, y, text, align, color, vertical = NULL) {
    lbl <- list(point = list(x = x, y = y, xAxis = 0, yAxis = 0), text = text, align = align, y = 20)
    if (!is.null(vertical)) lbl$verticalAlign <- vertical
    hc_add_annotation(
      hc,
      labelOptions = list(
        backgroundColor = "transparent", borderWidth = 0, useHTML = TRUE,
        crop = FALSE, overflow = "allow", padding = 0,
        style = list(fontSize = "13px", color = color, fontWeight = "bold")
      ),
      labels = list(lbl)
    )
  }

  highchart() |>
    hc_chart(type = "bubble", height = 600, style = list(fontFamily = "Arial")) |>
    hc_xAxis(
      title = list(text = paste0("Standortquotient (", max_year, ")", sq_help_icon), useHTML = TRUE),
      min = minx, max = maxx,
      plotLines = list(list(value = 1, color = "#c9c9c9", width = 2))
    ) |>
    hc_yAxis(
      title = list(text = paste0(
        "Mittleres jährliches Wachstum der Beschäftigten in % (", min_year, "-", max_year, ")"
      )),
      min = miny, max = maxy,
      plotLines = list(list(value = 0, color = "#c9c9c9", width = 2))
    ) |>
    hc_tooltip(
      headerFormat = "",
      formatter = JS("function() {
        return '<b>' + this.point.name + '</b><br>' +
               'Standortquotient: ' + this.point.x.toFixed(2) + '<br>' +
               'ø jährl. Wachstum: ' + this.point.y.toFixed(1) + ' %<br>' +
               'Beschäftigte: ' + Highcharts.numberFormat(this.point.z, 0, '', \"'\");
      }")
    ) |>
    hc_plotOptions(bubble = list(
      opacity = 0.9, minSize = "0.3%", maxSize = "20%", marker = list(lineWidth = 1)
    )) |>
    hc_series(list(showInLegend = FALSE, name = "", data = punkte)) |>
    quadrant_label(minx, maxy, "Beschäftigung gewachsen /<br>Branche schwach vertreten", "left", cols$label[["tl"]]) |>
    quadrant_label(maxx, maxy, "Beschäftigung gewachsen /<br>Branche stark vertreten", "right", cols$label[["tr"]]) |>
    quadrant_label(minx, miny + 0.5, "Beschäftigung gesunken /<br>Branche schwach vertreten", "left", cols$label[["bl"]], "bottom") |>
    quadrant_label(maxx, miny + 0.5, "Beschäftigung gesunken /<br>Branche stark vertreten", "right", cols$label[["br"]]) |>
    hc_quelle(max_year) |>
    hc_legend(
      enabled = TRUE, x = 0, align = "right", verticalAlign = "top", title = "",
      itemStyle = list(fontSize = "12px", fontWeight = "normal", maxWidth = "100%")
    ) |>
    # Legenden-Eintrag für die drei grössten Branchen
    hc_add_series(
      name = legend_text, type = "scatter", data = list(), showInLegend = TRUE,
      marker = list(symbol = "circle", fillColor = "transparent", lineWidth = 1, lineColor = "#000000", radius = 8)
    )
}

#' Dumbbell-Chart: Beschäftigte min_year vs. max_year
#' @noRd
create_dumbbell_chart <- function(data, max_year, min_year) {
  wachstum <- c(line = "#1D6AA2", low = "#bde1b4")
  rueckgang <- c(line = "#A73559", low = "#f5a951")

  data <- data |>
    arrange(desc(current)) |>
    mutate(
      sinkt = current < previous,
      low = previous,
      high = current,
      change = as.integer(round(current - previous)),
      color = ifelse(sinkt, rueckgang[["line"]], wachstum[["line"]]),
      highColor = color,
      lowColor = ifelse(sinkt, rueckgang[["low"]], wachstum[["low"]])
    )

  legend_dummy <- function(hc, name, index) {
    hc_add_series(
      hc, name = name, type = "scatter", data = list(c(NA, NA)),
      marker = list(enabled = FALSE), showInLegend = TRUE, legendIndex = index
    )
  }

  highchart() |>
    hc_chart(type = "dumbbell", inverted = TRUE, height = 450, marginRight = 20,
             style = list(fontFamily = "Arial")) |>
    hc_xAxis(categories = data$name) |>
    hc_yAxis(
      min = 0,
      max = max(data$high, data$low, na.rm = TRUE),
      title = list(text = "Anzahl Beschäftigte", align = "middle"),
      opposite = TRUE,
      labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 0, '', \"'\"); }"))
    ) |>
    hc_add_series(
      showInLegend = FALSE,
      name = paste("Daten", min_year, "im Vergleich mit", max_year),
      data = list_parse(data[, c("name", "low", "high", "change", "color", "lowColor", "highColor")]),
      lineWidth = 2,
      marker = list(enabled = TRUE, radius = 4),
      states = list(hover = list(lineWidth = 4))
    ) |>
    legend_dummy("Anstieg", 1) |>
    legend_dummy("Rückgang", 2) |>
    legend_dummy(as.character(min_year), 3) |>
    legend_dummy(as.character(max_year), 4) |>
    hc_tooltip(formatter = JS(paste0("function() {
      var changeText = this.point.change > 0 ? 'Zunahme' : 'Abnahme';
      var f = function(v) { return Highcharts.numberFormat(v, 0, '', \"'\"); };
      return '<b>' + this.point.name + '</b><br>' +
             '", min_year, ": ' + f(this.point.low) + '<br>' +
             '", max_year, ": ' + f(this.point.high) + '<br>' +
             changeText + ': ' + f(Math.abs(this.point.change));
    }"))) |>
    hc_quelle(max_year) |>
    hc_legend(
      enabled = TRUE, align = "right", verticalAlign = "top", layout = "horizontal",
      itemStyle = list(fontSize = "11px", fontWeight = "normal"),
      margin = 1, padding = 2, itemMarginRight = 15, useHTML = TRUE,
      symbolWidth = 0, symbolHeight = 0,
      labelFormatter = JS(sprintf("function() {
        var c = function(cx, fill) {
          return '<circle cx=\"' + cx + '\" cy=\"8\" r=\"6\" fill=\"' + fill + '\" stroke=\"#999\" stroke-width=\"0.5\"/>';
        };
        var item = function(w, circles, label) {
          return '<span style=\"display:inline-flex;align-items:center;\">' +
                 '<svg width=\"' + w + '\" height=\"16\" style=\"margin-right:5px;\">' + circles + '</svg>' +
                 label + '</span>';
        };
        switch (this.name) {
          case 'Anstieg':  return item(26, c(8, '%1$s') + c(16, '%2$s'), '<span>Anstieg</span>');
          case 'Rückgang': return item(26, c(16, '%3$s') + c(8, '%4$s'),
            '<span style=\"margin-right:30px;\">Rückgang</span><span style=\"color:#ccc;font-size:14px;\">|</span>');
          case '%5$s': return item(32, c(8, '%1$s') + c(24, '%3$s'), '<span>%5$s</span>');
          case '%6$s': return item(32, c(8, '%2$s') + c(24, '%4$s'), '<span>%6$s</span>');
        }
      }", wachstum[["low"]], wachstum[["line"]], rueckgang[["low"]], rueckgang[["line"]], min_year, max_year))
    )
}

#' Balkendiagramm: Standortquotient pro Branche
#' @noRd
create_bar_chart <- function(data, level, max_year) {
  cols <- chart_colors(level)

  data <- data |>
    arrange(desc(x)) |>
    mutate(
      y = round(x, 2),
      color = ifelse(y > 1, cols$over, cols$under),
      borderColor = color
    )

  legend_square <- function(hc, name, color, index) {
    hc_add_series(
      hc, name = name, type = "scatter", data = list(), color = color,
      showInLegend = TRUE, marker = list(symbol = "square", radius = 8), legendIndex = index
    )
  }

  highchart() |>
    hc_chart(type = "bar", height = 450, style = list(fontFamily = "Arial")) |>
    hc_xAxis(categories = data$name) |>
    hc_yAxis(
      title = list(text = paste0("Standortquotient (SQ) (", max_year, ")", sq_help_icon), useHTML = TRUE),
      opposite = TRUE,
      labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 1, '.', ' '); }")),
      plotLines = list(list(value = 1, color = "#c9c9c9", width = 2))
    ) |>
    hc_add_series(
      name = "Standortquotient", borderWidth = 1, showInLegend = FALSE,
      data = list_parse(data[, c("y", "color", "borderColor")])
    ) |>
    legend_square("Stark vertreten (SQ > 1)", cols$over, 1) |>
    legend_square("Schwach vertreten (SQ < 1)", cols$under, 2) |>
    hc_tooltip(pointFormat = "Standortquotient: <b>{point.y:.1f}</b>") |>
    hc_quelle(max_year) |>
    hc_legend(
      enabled = TRUE, title = "", align = "right", verticalAlign = "top", layout = "horizontal",
      itemStyle = list(fontSize = "11px", fontWeight = "normal"), symbolWidth = 12, symbolHeight = 12
    )
}
