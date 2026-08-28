# Fixed create_bar_chart function with custom legend and different colors for verarbeitendes gewerbe
create_bar_chart <- function(data, max_year, level_type = "kanton") {

  # Define different color sets based on level_type
  if(level_type == "c_sektor") {
    # Alternative color set for verarbeitendes gewerbe
    color_over <- "#ADD9A1"    # Dark green for over-represented (SQ > 1)
    color_under <- "#F5A951"   # Dark goldenrod for under-represented (SQ < 1)
    legend_over <- "Stark vertreten (SQ > 1)"
    legend_under <- "Schwach vertreten (SQ < 1)"
  } else {
    # Original color set for all other levels
    color_over <- "#4981b4"    # Blue for over-represented (SQ > 1)
    color_under <- "#9d5b99"   # Purple for under-represented (SQ < 1)
    legend_over <- "Stark vertreten (SQ > 1)"
    legend_under <- "Schwach vertreten (SQ < 1)"
  }

  # Prepare data with appropriate colors
  data <- data %>%
    arrange(desc(x)) %>%
    mutate(x = round(x, 2)) %>%
    mutate(
      color = ifelse(x > 1, color_over, color_under),
      borderColor = ifelse(x > 1, color_over, color_under)
    )

  # Datenstruktur für Highcharts anpassen
  data_list <- list_parse(data.frame(y = data$x, color = data$color, borderColor = data$borderColor))

  highchart() %>%
    hc_chart(type = "bar", height = 450, style = list(fontFamily = "Arial")) %>%
    hc_title(text = "", align = "left", x = 0) %>%
    hc_subtitle(text = "", align = "left", x = 0) %>%
    hc_xAxis(
      categories = data$name
    ) %>%
    hc_yAxis(
      title = list(
        text = paste0('Standortquotient (SQ) (', max_year, ')<i class="fas fa-question-circle help-icon" title="Der Standortquotient (SQ) zeigt, wie stark eine Branche in einer Region im Vergleich zum nationalen Durchschnitt vertreten ist. SQ > 1: überdurchschnittlich, SQ = 1: gleich wie der nationale Durchschnitt, SQ < 1: unterdurchschnittlich"></i>'),
        useHTML = TRUE
      ),
      opposite = TRUE,
      labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 1, '.', ' '); }")),
      plotLines = list(  # Hier wird die vertikale Linie bei x = 1 hinzugefügt
        list(
          value = 1,
          color = "#c9c9c9",
          width = 2
        )
      )
    ) %>%
    # Hauptserie (ohne Legend)
    hc_add_series(
      name = "Standortquotient",
      borderWidth = 1,
      data = data_list,
      showInLegend = FALSE
    ) %>%
    # Custom Legend Series 1: Übervertreten (SQ > 1)
    hc_add_series(
      name = legend_over,
      type = "scatter",
      data = list(),  # Empty data
      color = color_over,
      showInLegend = TRUE,
      marker = list(
        symbol = "square",
        radius = 8
      ),
      legendIndex = 1
    ) %>%
    # Custom Legend Series 2: Untervertreten (SQ < 1)
    hc_add_series(
      name = legend_under,
      type = "scatter",
      data = list(),  # Empty data
      color = color_under,
      showInLegend = TRUE,
      marker = list(
        symbol = "square",
        radius = 8
      ),
      legendIndex = 2
    ) %>%
    hc_tooltip(
      pointFormat = "Standortquotient: <b>{point.y:.1f}"
    ) %>%
    hc_credits(
      enabled = TRUE,
      text = paste0("Datenquelle: Bundesamt für Statistik, STATENT ", max_year -10, "-", max_year)
    ) %>%
    hc_legend(
      enabled = TRUE,
      title = "",
      align = "right",
      verticalAlign = "top",
      layout = "horizontal",
      itemStyle = list(fontSize = '11px', fontWeight = 'normal'),
      symbolWidth = 12,
      symbolHeight = 12
    )
}
