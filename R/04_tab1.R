create_dumbbell_chart <- function(data, max_value, max_year, min_year) {
  data <- data %>%
    arrange(desc(current)) %>%
    mutate(
      change = as.integer(round(current - previous, 0)),
      # Different colors based on growth/decline
      line_color = ifelse(current < previous, "#A73559", "#1D6AA2"),  # Red if declining, blue if growing
      high_color = ifelse(current < previous, "#A73559", "#1D6AA2"),  # Darker red/blue for current value
      low_color = ifelse(current < previous, "#f5a951", "#bde1b4")    # Lighter red/blue for previous value
    )

  highchart() %>%
    hc_chart(
      type = "dumbbell",
      inverted = TRUE,
      height = 450,
      style = list(fontFamily = "Arial"),
      marginRight = 20  # Add right margin for y-axis labels
    ) %>%
    hc_title(text = "", align = "left", x = 0) %>%
    hc_subtitle(
      text = "",
      align = "left",
      x = 0,
      padding = c(5, 0, 5, 0)  # top, right, bottom, left
    ) %>%
    hc_xAxis(categories = data$name)  %>%
    hc_yAxis(
      min = 0,
      max = max_value,
      title = list(text = "Anzahl Beschäftigte", align = "middle"),
      opposite = TRUE,
      labels = list(formatter = JS("function() { return Highcharts.numberFormat(this.value, 0, '', \"'\"); }"))
    ) %>%
    hc_add_series(
      showInLegend = FALSE,
      name = paste("Daten", min_year, "im Vergleich mit", max_year),
      data = list_parse(data %>%
                          select(previous, current, change, line_color, name, high_color, low_color) %>%
                          mutate(
                            low = previous,
                            high = current,
                            color = line_color,      # Color for the connecting line
                            lowColor = low_color,    # Color for previous value marker
                            highColor = high_color   # Color for current value marker
                          )),
      lineWidth = 2,
      marker = list(enabled = TRUE, radius = 4),
      states = list(hover = list(lineWidth = 4))
    ) %>%
    # Legend items - all with custom overlapping circles
    hc_add_series(
      name = paste("Anstieg"),
      type = "scatter",
      data = list(c(NA, NA)),
      marker = list(enabled = FALSE),  # Disable default marker
      showInLegend = TRUE,
      legendIndex = 1
    ) %>%
    hc_add_series(
      name = paste("Rückgang"),
      type = "scatter",
      data = list(c(NA, NA)),
      marker = list(enabled = FALSE),  # Disable default marker
      showInLegend = TRUE,
      legendIndex = 2
    ) %>%
    hc_add_series(
      name = paste(min_year),
      type = "scatter",
      data = list(c(NA, NA)),
      marker = list(enabled = FALSE),  # Disable default marker
      showInLegend = TRUE,
      legendIndex = 3
    ) %>%
    hc_add_series(
      name = paste(max_year),
      type = "scatter",
      data = list(c(NA, NA)),
      marker = list(enabled = FALSE),  # Disable default marker
      showInLegend = TRUE,
      legendIndex = 4
    ) %>%
    hc_tooltip(
      formatter = JS(paste0("function() {
        var changeText = this.point.change > 0 ? 'Zunahme' : 'Abnahme';
        return '<b>' + this.point.name + '</b><br>' +
               '", min_year, ": ' + Highcharts.numberFormat(this.point.low, 0, '', \"'\") + '<br>' +
               '", max_year, ": ' + Highcharts.numberFormat(this.point.high, 0, '', \"'\") + '<br>' +
               changeText + ': ' + Highcharts.numberFormat(Math.abs(this.point.change), 0, '', \"'\");
      }"))
    ) %>%
    hc_credits(
      enabled = TRUE,
      text = paste0("Datenquelle: Bundesamt für Statistik, STATENT ", max_year -10, "-", max_year)
    ) %>%
    hc_legend(
      enabled = TRUE,
      align = "right",
      verticalAlign = "top",
      layout = "horizontal",
      itemStyle = list(fontSize = "11px", fontWeight = "normal"),
      margin = 1,
      padding = 2,
      itemMarginRight = 15,  # Add gap between legend items
      useHTML = TRUE,
      symbolWidth = 0,  # Remove default symbol space
      symbolHeight = 0,  # Remove default symbol space
      labelFormatter = JS(paste0("function() {
        var lightColor, darkColor;

        // Different legend styles for different items
        if (this.name.includes('Anstieg')) {
          // Overlapping circles for Anstieg
          lightColor = '#bde1b4';  // Light green
          darkColor = '#1D6AA2';   // Dark blue
          return '<span style=\"display: inline-flex; align-items: center;\">' +
                 '<svg width=\"26\" height=\"16\" style=\"margin-right: 5px;\">' +
                 '<circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"' + lightColor + '\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '<circle cx=\"16\" cy=\"8\" r=\"6\" fill=\"' + darkColor + '\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '</svg>' +
                 '<span>' + this.name + '</span>' +
                 '</span>';
        } else if (this.name.includes('Rückgang')) {
          // Overlapping circles for Rückgang with separator line after
          lightColor = '#F5A951';  // Light red/orange
          darkColor = '#A73559';   // Dark red
          return '<span style=\"display: inline-flex; align-items: center;\">' +
                 '<svg width=\"26\" height=\"16\" style=\"margin-right: 5px;\">' +
                 '<circle cx=\"16\" cy=\"8\" r=\"6\" fill=\"' + lightColor + '\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '<circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"' + darkColor + '\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '</svg>' +
                 '<span style=\"margin-right: 30px;\">' + this.name + '</span>' +
                 '<span style=\"color: #ccc; font-size: 14px; margin-right: 0px;\">|</span>' +
                 '</span>';
        } else if (this.name.includes('", min_year, "')) {
          // Two separate circles for min_year
          return '<span style=\"display: inline-flex; align-items: center;\">' +
                 '<svg width=\"32\" height=\"16\" style=\"margin-right: 5px;\">' +
                 '<circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"#bde1b4\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '<circle cx=\"24\" cy=\"8\" r=\"6\" fill=\"#F5A951\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '</svg>' +
                 '<span>' + this.name + '</span>' +
                 '</span>';
        } else if (this.name.includes('", max_year, "')) {
          // Two separate circles for max_year
          return '<span style=\"display: inline-flex; align-items: center;\">' +
                 '<svg width=\"32\" height=\"16\" style=\"margin-right: 5px;\">' +
                 '<circle cx=\"8\" cy=\"8\" r=\"6\" fill=\"#1D6AA2\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '<circle cx=\"24\" cy=\"8\" r=\"6\" fill=\"#A73559\" stroke=\"#999\" stroke-width=\"0.5\"/>' +
                 '</svg>' +
                 '<span>' + this.name + '</span>' +
                 '</span>';
        }
      }"))
    )
}
