create_bubble_chart <- function(data, size, minx, miny, maxx, maxy, level_type = "kanton", max_year, min_year, level_name = "", legend_text = "Die grössten drei Branchen") {

  if(all(c("zehn_jahr_wachstum_gemeinde", "Standortquotient_gemeinde") %in% names(data))) {
    data <- data %>%
      filter(!is.na(zehn_jahr_wachstum_gemeinde) & !is.na(Standortquotient_gemeinde))
  }

  # Determine the correct preposition and location text
  location_text <- switch(level_type,
                          "kanton" = "im Kanton Thurgau",
                          "bezirk" = paste("im Bezirk", level_name),
                          "gemeinde" = paste("in der Gemeinde", level_name),
                          "c_sektor" = "im Kanton Thurgau"
  )

  titel_text <- switch(level_type,
                       "kanton" = "Im Kanton Thurgau ",
                       "bezirk" = "Im Bezirk ",
                       "gemeinde" = "In der Gemeinde ",
                       "c_sektor" = "Im Kanton Thurgau "
  )

  # Define different color sets based on level_type
  if(level_type == "c_sektor") {
    # Alternative color set for verarbeitendes gewerbe
    data <- data %>%
      mutate(
        color = case_when(
          x > 1 & y > 0 ~ "#8BAF81",   # Dark green for x > 1 and y > 0
          x > 1 & y <= 0 ~ "#c9e7c0",  # Light green for x > 1 and y <= 0
          x <= 1 & y > 0 ~ "#DC9848",  # Dark goldenrod for x <= 1 and y > 0
          x <= 1 & y <= 0 ~ "#f8cfa7"  # Light khaki for x <= 1 and y <= 0
        ),
        x = round(x,2),
        y = round(y,2)
      )

    # Define annotation colors for c_sektor
    annotation_colors <- list(
      top_left = "#DC9848",     # Dark goldenrod
      top_right = "#8BAF81",    # Dark green
      bottom_left = "#f8cfa7",  # Light khaki
      bottom_right = "#c9e7c0"  # Light green
    )
  } else {
    # Original color set for all other levels
    data <- data %>%
      mutate(
        color = case_when(
          x > 1 & y > 0 ~ "#1d6aa2",   # Dunkelblau für x > 1 und y > 0
          x > 1 & y <= 0 ~ "#aacbec",  # Hellblau für x > 1 und y <= 0
          x <= 1 & y > 0 ~ "#863d83",  # Orange für x <= 1 und y > 0
          x <= 1 & y <= 0 ~ "#cea9cc"  # Hellorange für x <= 1 und y <= 0
        ),
        x = round(x,2),
        y = round(y,2)
      )

    # Define annotation colors for other levels
    annotation_colors <- list(
      top_left = "#863d83",     # Original purple/orange
      top_right = "#1d6aa2",    # Original dark blue
      bottom_left = "#c39ec1",  # Original light purple
      bottom_right = "#8ab2d9"  # Original light blue
    )
  }

  # Identify the top 3 largest bubbles (by z value)
  data <- data %>%
    arrange(desc(z)) %>%
    mutate(
      is_top3 = row_number() <= 3
    )

  highchart() %>%
    hc_chart(type = "bubble", height = 600, style = list(fontFamily = "Arial")) %>%
    hc_xAxis(
      title = list(
        text = paste0('Standortquotient (',max_year,')<i class="fas fa-question-circle help-icon" title="Der Standortquotient (SQ) zeigt, wie stark eine Branche in einer Region im Vergleich zum nationalen Durchschnitt vertreten ist. SQ > 1: überdurchschnittlich, SQ = 1: gleich wie der nationale Durchschnitt, SQ < 1: unterdurchschnittlich"></i>'),
        useHTML = TRUE
      ),
      min = minx,
      max = maxx,
      plotLines = list(
        list(
          value = 1,
          color = "#c9c9c9",
          width = 2
        )
      )
    ) %>%
    hc_yAxis(
      title = list(
        text = paste0('Mittleres jährliches Wachstum der Beschäftigten in % (', min_year, '-', max_year, ')'),
        useHTML = TRUE
      ),
      min = miny,
      max = maxy,
      plotLines = list(
        list(
          value = 0,
          color = "#c9c9c9",
          width = 2
        )
      )
    ) %>%
    hc_tooltip(
      headerFormat = "",
      formatter = JS("function() {
        return '<b>' + this.point.name + '</b><br>' +
               'Standortquotient: ' + this.point.x.toFixed(2) + '<br>' +
               'ø jährl. Wachstum: ' + this.point.y.toFixed(1) + ' %<br>' +
               'Beschäftigte: ' + Highcharts.numberFormat(this.point.z, 0, '', \"'\");
      }")
    ) %>%
    hc_plotOptions(bubble = list(
      opacity = 0.9,
      maxSize = size,
      marker = list(lineWidth = 1)
    )) %>%
    hc_series(
      list(
        showInLegend = FALSE,
        name = "",
        data = mapply(function(x, y, z, name, color, is_top3) {
          list(
            x = round(x,2),
            y = round(y,2),
            z = z,
            name = name,
            color = color,
            # Add highlighting for top 3 bubbles
            marker = if(is_top3) {
              list(
                lineWidth = 1,           # Thicker border
                lineColor = "#000000"    # Black border color
              )
            } else {
              list(lineWidth = 1)        # Normal border for others
            },
            dataLabels = list(
              enabled = is_top3,
              format = '{point.name}',
              style = list(
                color = "#000000",
                fontWeight = 'bold',
                fontSize = "11px",
                textOutline = '2px white'
              ),
              verticalAlign = 'bottom',
              horizontalAlign = 'left',
              x = 0,
              y = 10
            )
          )
        }, data$x, data$y, data$z, data$name, data$color, data$is_top3, SIMPLIFY = FALSE)
      )
    ) %>%
    hc_add_annotation(
      labelOptions = list(
        backgroundColor = "transparent",
        borderWidth = 0,
        style = list(
          fontSize = "13px",
          color = annotation_colors$top_left,
          fontWeight = "bold"
        ),
        useHTML = TRUE,
        crop = FALSE,
        overflow = "allow",
        padding = 0
      ),
      labels = list(
        list(
          point = list(
            x = minx,
            y = maxy,
            xAxis = 0,
            yAxis = 0
          ),
          text = "Beschäftigung gewachsen /<br>Branche schwach vertreten",
          align= "left",
          y = 20
        )
      )
    ) %>%
    hc_add_annotation(
      labelOptions = list(
        backgroundColor = "transparent",
        borderWidth = 0,
        style = list(
          fontSize = "13px",
          color = annotation_colors$top_right,
          fontWeight = "bold"
        ),
        useHTML = TRUE,
        crop = FALSE,
        overflow = "allow",
        padding = 0
      ),
      labels = list(
        list(
          point = list(
            x = maxx,
            y = maxy,
            xAxis = 0,
            yAxis = 0
          ),
          text = "Beschäftigung gewachsen /<br>Branche stark vertreten",
          align= "right",
          y = 20
        )
      )
    ) %>%
    hc_add_annotation(
      labelOptions = list(
        backgroundColor = "transparent",
        borderWidth = 0,
        style = list(
          fontSize = "13px",
          color = annotation_colors$bottom_left,
          fontWeight = "bold"
        ),
        useHTML = TRUE,
        crop = FALSE,
        overflow = "allow",
        padding = 0
      ),
      labels = list(
        list(
          point = list(
            x = minx,
            y = miny + 0.5,
            xAxis = 0,
            yAxis = 0
          ),
          text = "Beschäftigung gesunken /<br>Branche schwach vertreten",
          align= "left",
          verticalAlign = "bottom",
          y = 20
        )
      )
    ) %>%
    hc_add_annotation(
      labelOptions = list(
        backgroundColor = "transparent",
        borderWidth = 0,
        style = list(
          fontSize = "13px",
          color = annotation_colors$bottom_right,
          fontWeight = "bold"
        ),
        useHTML = TRUE,
        crop = FALSE,
        overflow = "allow",
        padding = 0
      ),
      labels = list(
        list(
          point = list(
            x = maxx,
            y = miny + 0.5,
            xAxis = 0,
            yAxis = 0
          ),
          text = "Beschäftigung gesunken /<br>Branche stark vertreten",
          align = "right",
          y = 20
        )
      )
    ) %>%
    hc_credits(
      enabled = TRUE,
      text = paste0("Datenquelle: Bundesamt für Statistik, STATENT ", max_year -10, "-", max_year)
    ) %>%
    hc_legend(
      enabled = TRUE,
      x = 0,
      align = "right",
      verticalAlign = "top",
      title = "",
      itemStyle = list(
        fontSize = '12px',
        fontWeight = 'normal',  # This ensures normal weight for non-bold text
        maxWidth = '100%'
      ),
      itemWidth = NULL
    ) %>%
    # Add active legend entry for Top 3 Branches only
    hc_add_series(
      name = legend_text,
      type = "scatter",
      data = list(),  # Empty data so nothing shows on chart
      showInLegend = TRUE,
      visible = TRUE,  # Make it active/clickable
      marker = list(
        symbol = "circle",
        fillColor = "transparent",
        lineWidth = 1,
        lineColor = "#000000",
        radius = 8
      )
    )
}
