rm(list=ls())

# Source all R files from the R directory
sapply(list.files("/r-proj/stat/ogd/branchenstruktur/Beschäftigte nach NOGA nach Gemeinde/App/R/",
                  pattern = "\\.R$", full.names = TRUE), source)

daten_kanton <- read_xlsx("Daten/daten_kanton_use.xlsx")
daten_bezirk <- read_xlsx("Daten/daten_bezirk_use.xlsx")
daten_gemeinde <- read_xlsx("Daten/daten_gemeinde_use.xlsx",
                            col_types = c("numeric", "text", "text", "text", "text", "numeric", "numeric", "numeric"))
daten_sectorC <- read_xlsx("Daten/daten_sectorC_use.xlsx")

# Define years for use in UI
max_year <- max(daten_kanton$jahr, na.rm = TRUE)
min_year <- max_year - 10

###################################################################################
# PREPARE DATA FOR CHARTS
###################################################################################

# Fixed prepare_chart_data function with correct column names
prepare_chart_data <- function(df, level_type) {

  # Filter for the years we need
  current_data <- df %>% filter(jahr == max_year)
  previous_data <- df %>% filter(jahr == min_year)

  if(level_type == "gemeinde") {
    # For Gemeinden - group by gemeinde
    gemeinde_list <- unique(df$gemeinde)

    datasets_GM <- list()

    for(gem in gemeinde_list) {
      current_gem <- current_data %>% filter(gemeinde == gem)
      previous_gem <- previous_data %>% filter(gemeinde == gem)

      if(nrow(current_gem) > 0 && nrow(previous_gem) > 0) {
        chart_data <- current_gem %>%
          left_join(previous_gem, by = "noga_section_code", suffix = c("_current", "_previous")) %>%
          mutate(
            name = noga_section_text_lower_current,
            current = beschaeftigte_total_current,
            previous = beschaeftigte_total_previous,
            x = Standortquotient_gemeinde_current,  # Fixed: use _current suffix
            y = zehn_jahr_wachstum_gemeinde_current,  # Fixed: use _current suffix
            z = beschaeftigte_total_current,
            low = pmin(previous, current, na.rm = TRUE),
            high = pmax(previous, current, na.rm = TRUE),
            change = as.integer(current - previous)
          ) %>%
          select(name, current, previous, x, y, z, low, high, change) %>%
          filter(!is.na(current), !is.na(previous))

        if(nrow(chart_data) > 0) {
          datasets_GM[[gem]] <- chart_data
        }
      }
    }
    return(datasets_GM)

  } else if(level_type == "bezirk") {
    # For Bezirke
    bezirk_list <- unique(df$bezirk[!is.na(df$bezirk)])

    datasets <- list()

    for(bez in bezirk_list) {
      current_bez <- current_data %>% filter(bezirk == bez)
      previous_bez <- previous_data %>% filter(bezirk == bez)

      if(nrow(current_bez) > 0 && nrow(previous_bez) > 0) {
        chart_data <- current_bez %>%
          left_join(previous_bez, by = "noga_section_code", suffix = c("_current", "_previous")) %>%
          mutate(
            name = noga_section_text_lower_current,
            current = beschaeftigte_total_current,
            previous = beschaeftigte_total_previous,
            x = Standortquotient_bezirk_current,  # Fixed: use _current suffix
            y = zehn_jahr_wachstum_bezirk_current,  # Fixed: use _current suffix
            z = beschaeftigte_total_current,
            low = pmin(previous, current, na.rm = TRUE),
            high = pmax(previous, current, na.rm = TRUE),
            change = as.integer(current - previous)
          ) %>%
          select(name, current, previous, x, y, z, low, high, change) %>%
          filter(!is.na(current), !is.na(previous))

        if(nrow(chart_data) > 0) {
          datasets[[bez]] <- chart_data
        }
      }
    }
    return(datasets)

  } else if(level_type == "kanton") {
    # For Kanton (TG level)
    current_tg <- current_data
    previous_tg <- previous_data

    if(nrow(current_tg) > 0 && nrow(previous_tg) > 0) {
      chart_data <- current_tg %>%
        left_join(previous_tg, by = "noga_section_code", suffix = c("_current", "_previous")) %>%
        mutate(
          name = noga_section_text_lower_current,
          current = beschaeftigte_total_current,
          previous = beschaeftigte_total_previous,
          x = Standortquotient_tg_current,  # Fixed: use _current suffix
          y = zehn_jahr_wachstum_tg_current,  # Fixed: use _current suffix
          z = beschaeftigte_total_current,
          low = pmin(previous, current, na.rm = TRUE),
          high = pmax(previous, current, na.rm = TRUE),
          change = as.integer(current - previous)
        ) %>%
        select(name, current, previous, x, y, z, low, high, change) %>%
        filter(!is.na(current), !is.na(previous))

      return(list("Thurgau" = chart_data))
    }

  } else if(level_type == "c_sektor") {
    # For C Sektor (manufacturing)
    current_c <- current_data
    previous_c <- previous_data

    if(nrow(current_c) > 0 && nrow(previous_c) > 0) {
      chart_data <- current_c %>%
        left_join(previous_c, by = "noga_zweisteller_code", suffix = c("_current", "_previous")) %>%
        mutate(
          name = noga_zweisteller_text_current,
          current = beschaeftigte_total_current,
          previous = beschaeftigte_total_previous,
          x = Standortquotient_tg_c_current,  # Fixed: use _current suffix
          y = zehn_jahr_wachstum_tg_current,  # Fixed: use _current suffix
          z = beschaeftigte_total_current,
          low = pmin(previous, current, na.rm = TRUE),
          high = pmax(previous, current, na.rm = TRUE),
          change = as.integer(current - previous)
        ) %>%
        select(name, current, previous, x, y, z, low, high, change) %>%
        filter(!is.na(current), !is.na(previous))

      return(list("Thurgau_nur_C" = chart_data))
    }
  }

  return(list())
}

# Prepare all datasets
datasets_GM <- prepare_chart_data(daten_gemeinde, "gemeinde")
datasets <- prepare_chart_data(daten_bezirk, "bezirk")
datasets_TG <- prepare_chart_data(daten_kanton, "kanton")
datasets_C <- prepare_chart_data(daten_sectorC, "c_sektor")

# Combine TG datasets with C sektor data
datasets_TG <- c(datasets_TG, datasets_C)

###################################################################################
# UI COMPONENTS
###################################################################################

# Navbar content - REMOVED TAB 4
navbar_content <- tags$ul(class = "navbar-nav ml-auto",
                          tags$li(class = "nav-item",
                                  tags$a(class = "nav-link", href = "#", onclick = "Shiny.setInputValue('navbar_tab', 'tab1')",
                                         "Kanton")
                          ),
                          tags$li(class = "nav-item",
                                  tags$a(class = "nav-link", href = "#", onclick = "Shiny.setInputValue('navbar_tab', 'tab2')",
                                         "Bezirke")
                          ),
                          tags$li(class = "nav-item",
                                  tags$a(class = "nav-link", href = "#", onclick = "Shiny.setInputValue('navbar_tab', 'tab3')",
                                         "Gemeinden")
                          )
)

# Header
db_header <- init_header(
  dashboard_title = "Amt für Daten und Statistik",
  reference = 'https://statistik.tg.ch'
)

db_content <- div(
  # Tab 1 - Kanton (Default landing page) - MODIFIED WITH EXPANDABLE SECTION
  # Tab 1 - Kanton section - modify the fluidRow containing the Lesebeispiel
  conditionalPanel(
    condition = "input.navbar_tab == 'tab1' || typeof input.navbar_tab == 'undefined'",
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 0px; padding: 0px;",
          div(
            style = "margin-bottom: 5px; float: right;",
            actionButton("btn_bubble_tg", "Übersicht",
                         class = "btn btn-primary nav-button"),
            actionButton("btn_dumbbell_tg", "Detail: Grösse und Wachstum",
                         class = "btn btn-outline-primary nav-button"),
            actionButton("btn_bar_tg", "Detail: Standortquotient",
                         class = "btn btn-outline-primary nav-button")
          ),
          # Dynamic chart description
          uiOutput("dynamic_description_tg"),
          # Single dynamic chart output
          highchartOutput("dynamic_chart_tg", height = "100%")
        )
      )
    ),
    # CONDITIONAL Lesebeispiel - only show for bubble chart (Übersicht)
    conditionalPanel(
      condition = "output.current_chart_is_bubble == true",
      fluidRow(
        column(
          width = 12,
          div(
            class = "lesebeispiel",
            HTML(paste0('<p>Die Visualisierung zeigt, </p><ul><li>welches die <strong>grössten</strong> Branchen im Kanton Thurgau sind (Je grösser der Bubble, desto mehr Beschäftigte arbeiten in der Branche. Die drei grössten Branchen sind schwarz umrandet.)</li>
                                      <li>welches die <strong>wachstumsstärksten</strong>  Branchen im Kanton Thurgau sind (Je weiter oben der Bubble, desto stärker ist die Branche in den letzten Jahren gewachsen.)</li>
                                      <li>welches die Branchen sind, die im Kanton Thurgau <strong>vergleichsweise stark vertreten</strong> sind (Je weiter rechts der Bubble, desto stärker ist die Branche im Vergleich zur Schweiz vertreten. Ein Standortquotienten von über 1 bedeutet: In dieser Branche arbeiten im Kanton Thurgau verhältnismässig mehr Beschäftigte als in der Gesamtschweiz).</li></ul>
                                      <p class="secondp">Im <strong>oberen rechten Quadranten</strong> sind die <strong>Wachstumsbranchen</strong> dargestellt, die im Kanton Thurgau im Vergleich zur Gesamtschweiz <strong>stärker vertreten</strong> sind.</p><br>
                        <p><strong>Lesebeispiel:</strong> Das Gesundheits- und Sozialwesen im Kanton Thurgau hatte im Jahr ', max_year,' 23\'033 Beschäftigte. Es gehört zu den drei grössten Branchen im Thurgau (grosser Bubble, schwarz umrandet). Es ist in den letzten Jahren stark gewachsen (+2.8 % im Schnitt der Jahre ', min_year, '-', max_year,'). Im Vergleich zur Gesamtschweiz arbeiten im Thurgau überdurchschnittlich viele Beschäftigte in dieser Branche (Standortquotient von 1,06).'))
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "button-section-container",
          h5("Daten und mehr zur Branchenstruktur", class = "button-section-title"),
          div(
            class = "button-group-horizontal",
            actionButton("btn_stat_kt", "Webartikel zum Thema",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit/arbeit-und-erwerb/beschaeftigte.html/6019/l/de/', '_blank')"),
            actionButton("btn_ogd_kt", "Daten als OGD",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://data.tg.ch/explore/dataset/sk-stat-150/information/', '_blank')"),
            downloadButton("download_KT", "Daten herunterladen",
                           class = "btn btn-outline-primary info-button"))),
        div(
          class = "button-section-container",
          h5("Weitere Informationen", class = "button-section-title"),
          div(class = "button-group-horizontal",
              actionButton("btn_ws_kt", "Wirtschaftsstatistiken",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit.html/4433', '_blank')"),
              actionButton("btn_wb_kt", "Wirtschaftsbarometer",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://wirtschaftsbarometer.tg.ch/', '_blank')"),
              actionButton("btn_glossar_kt", "Glossar",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/glossar/glossar.html/6736', '_blank')")

          )
        )
      )
    ),

    # Zusätzlich Verarbeitendes Gewerbe
      fluidRow(
        column(
          width = 12,
          div(class = "gewerbe_zoom",
              HTML("<p>Zoom auf das verarbeitende Gewerbe</p><hr>")),
          div(
            style = "margin-bottom: 0px; padding: 0px;",
            div(
              style = "margin-bottom: 5px; float: right;",
              actionButton("btn_bubble_gewerbe", "Übersicht",
                           class = "btn btn-primary nav-button"),
              actionButton("btn_dumbbell_gewerbe", "Detail: Grösse und Wachstum",
                           class = "btn btn-outline-primary nav-button"),
              actionButton("btn_bar_gewerbe", "Detail: Standortquotient",
                           class = "btn btn-outline-primary nav-button")
            ),
            # Dynamic chart description for Gewerbe
            uiOutput("dynamic_description_gewerbe"),
            # Chart output for Gewerbe
            highchartOutput("dynamic_chart_gewerbe", height = "100%")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "button-section-container",
            h5("Daten und mehr zur Branchenstruktur", class = "button-section-title"),
            div(
              class = "button-group-horizontal",
              actionButton("btn_stat_gewerbe", "Webartikel zum Thema",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit/arbeit-und-erwerb/beschaeftigte.html/6019/l/de/', '_blank')"),
              actionButton("btn_ogd_gewerbe", "Daten als OGD",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://data.tg.ch/explore/dataset/sk-stat-150/information/', '_blank')"),
              downloadButton("download_gewerbe", "Daten herunterladen",
                             class = "btn btn-outline-primary info-button"))),
          div(
            class = "button-section-container",
            h5("Weitere Informationen", class = "button-section-title"),
            div(class = "button-group-horizontal",
                actionButton("btn_ws_gewerbe", "Wirtschaftsstatistiken",
                             class = "btn btn-outline-secondary info-button",
                             onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit.html/4433', '_blank')"),
                actionButton("btn_wb_gewerbe", "Wirtschaftsbarometer",
                             class = "btn btn-outline-secondary info-button",
                             onclick = "window.open('https://wirtschaftsbarometer.tg.ch/', '_blank')"),
                actionButton("btn_glossar_gewerbe", "Glossar",
                             class = "btn btn-outline-secondary info-button",
                             onclick = "window.open('https://statistik.tg.ch/glossar/glossar.html/6736', '_blank')")

            )
          )
        )
      )
  ),

  # Tab 2 - Bezirke
  conditionalPanel(
    condition = "input.navbar_tab == 'tab2'",
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          # Left side: Select input
          div(
            class = "select-container",
            selectInput(
              "dataset_choice",
              NULL,
              choices = c("Wählen Sie Ihren Bezirk..." = "", names(datasets)),
              selected = ""
            )
          ),
          # Right side: Buttons
          div(
            style = "flex: 0 0 auto;",
            actionButton("btn_bubble_bezirke", "Übersicht",
                         class = "btn btn-primary nav-button"),
            actionButton("btn_dumbbell_bezirke", "Detail: Grösse und Wachstum",
                         class = "btn btn-outline-primary nav-button"),
            actionButton("btn_bar_bezirke", "Detail: Standortquotient",
                         class = "btn btn-outline-primary nav-button")
          )
        ),
        # Dynamic chart description
        uiOutput("dynamic_description_bezirke"),
        # Chart output
        div(
          highchartOutput("dynamic_chart_bezirke", height = "100%")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "button-section-container",
          h5("Daten und mehr zur Branchenstruktur", class = "button-section-title"),
          div(
            class = "button-group-horizontal",
            actionButton("btn_stat_bezirke", "Webartikel zum Thema",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit/arbeit-und-erwerb/beschaeftigte.html/6019/l/de/', '_blank')"),
            actionButton("btn_ogd_bezirke", "Daten als OGD",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://data.tg.ch/explore/dataset/sk-stat-150/information/', '_blank')"),
            downloadButton("download_bezirke", "Daten herunterladen",
                           class = "btn btn-outline-primary info-button"))),
        div(
          class = "button-section-container",
          h5("Weitere Informationen", class = "button-section-title"),
          div(class = "button-group-horizontal",
              actionButton("btn_ws_bezirke", "Wirtschaftsstatistiken",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit.html/4433', '_blank')"),
              actionButton("btn_wb_bezirke", "Wirtschaftsbarometer",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://wirtschaftsbarometer.tg.ch/', '_blank')"),
              actionButton("btn_glossar_bezirke", "Glossar",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/glossar/glossar.html/6736', '_blank')")

          )
        )
      )
    )
  ),

  # Tab 3 - Gemeinden
  conditionalPanel(
    condition = "input.navbar_tab == 'tab3'",
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          # Left side: Select input
          div(
            class = "select-container",
            selectInput(
              "dataset_choice_GM",
              NULL,
              choices = c("Wählen Sie Ihre Gemeinde..." = "", names(datasets_GM)),
              selected = ""
            )
          ),
          # Right side: Buttons
          div(
            style = "flex: 0 0 auto;",
            actionButton("btn_bubble_gemeinden", "Übersicht",
                         class = "btn btn-primary nav-button"),
            actionButton("btn_dumbbell_gemeinden", "Detail: Grösse und Wachstum",
                         class = "btn btn-outline-primary nav-button"),
            actionButton("btn_bar_gemeinden", "Detail: Standortquotient",
                         class = "btn btn-outline-primary nav-button")
          )
        ),
        # Dynamic chart description
        uiOutput("dynamic_description_gemeinden"),
        # Chart output
        div(
          highchartOutput("dynamic_chart_gemeinden", height = "100%")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "button-section-container",
          h5("Daten und mehr zur Branchenstruktur", class = "button-section-title"),
          div(
            class = "button-group-horizontal",
            actionButton("btn_stat_gemeinden", "Webartikel zum Thema",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit/arbeit-und-erwerb/beschaeftigte.html/6019/l/de/', '_blank')"),
            actionButton("btn_ogd_gemeinden", "Daten als OGD",
                         class = "btn btn-outline-secondary info-button",
                         onclick = "window.open('https://data.tg.ch/explore/dataset/sk-stat-150/information/', '_blank')"),
            downloadButton("download_gemeinden", "Daten herunterladen",
                           class = "btn btn-outline-primary info-button"))),
        div(
          class = "button-section-container",
          h5("Weitere Informationen", class = "button-section-title"),
          div(class = "button-group-horizontal",
              actionButton("btn_ws_gemeinden", "Wirtschaftsstatistiken",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/themen-und-daten/wirtschaft-und-arbeit.html/4433', '_blank')"),
              actionButton("btn_wb_gemeinden", "Wirtschaftsbarometer",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://wirtschaftsbarometer.tg.ch/', '_blank')"),
              actionButton("btn_glossar_gemeinden", "Glossar",
                           class = "btn btn-outline-secondary info-button",
                           onclick = "window.open('https://statistik.tg.ch/glossar/glossar.html/6736', '_blank')")

          )
        )
        )
    )
  )
)

# Initialize Dashboard Body
db_body <- init_body(
  db_content = db_content,
  navbar_content = navbar_content
)

ui <- dashboardPage(
  title = "Branchen",
  dark = NULL,
  help = NULL,
  header = db_header,
  sidebar = dashboardSidebar(disable = TRUE),
  body = db_body
)


###################################################################################
# SERVER
###################################################################################

server <- function(input, output, session){

  # Initialize navbar_tab to tab1 (Kanton) if not set
  observe({
    if(is.null(input$navbar_tab)) {
      updateTabItems(session, "navbar_tab", "tab1")
    }
  })

  # Reactive values to track current chart type for each tab
  current_chart_tg <- reactiveVal("bubble")
  current_chart_bezirke <- reactiveVal("bubble")
  current_chart_gemeinden <- reactiveVal("bubble")
  current_chart_gewerbe <- reactiveVal("bubble")

  output$current_chart_is_bubble <- reactive({
    current_chart_tg() == "bubble"
  })
  outputOptions(output, "current_chart_is_bubble", suspendWhenHidden = FALSE)

  # ========== DYNAMIC DESCRIPTIONS ==========

  # Tab 1 - Kanton descriptions
  output$dynamic_description_tg <- renderUI({
    chart_type <- current_chart_tg()

    content <- switch(chart_type,
                      "bubble" = list(
                        title = paste0("Die Branchenstruktur im Kanton Thurgau")),
                      "dumbbell" = list(
                        title = paste0("So haben sich die Beschäftigtenzahlen im Kanton Thurgau in den letzten zehn Jahren (", min_year, "-", max_year,") verändert"),
                        text = paste0('Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Branchen des Kantons Thurgau im Zehnjahreszeitraum von ', min_year, ' bis ', max_year,'. Die Branchen sind nach den Beschäftigtenzahlen im Jahr ', max_year,' geordnet.')
                      ),
                      "bar" = list(
                        title = paste0('Die Grösse der Thurgauer Branchen im Vergleich zur Gesamtschweiz'),
                        text = paste0('Die blauen Balken zeigen Branchen, in denen im Kanton Thurgau verhältnismässig mehr Beschäftigte arbeiten als in der Gesamtschweiz (Standortquotient > 1). Die violetten Balken zeigen Branchen, die im Kanton Thurgau im Vergleich zur Gesamtschweiz schwächer vertreten sind (Standortquotient < 1).')
                      )
    )

    div(
      class = "chart-description",
      h4(content$title),
      HTML(paste0('<p>', content$text, '</p>'))
    )
  })



  # Verarbeitendes Gewerbe descriptions
  output$dynamic_description_gewerbe <- renderUI({
    chart_type <- current_chart_gewerbe()

    content <- switch(chart_type,
                      "bubble" = list(
                        title = paste0("Das verarbeitende Gewerbe ist die grösste Branche im Kanton Thurgau"),
                        text = paste0('Das verarbeitende Gewerbe ist die beschäftigungsstärkste Branche im Kanton Thurgau. Diese Grafik zeigt, welches innerhalb des Verarbeitenden Gewerbes die grössten Subbranchen sind, welche Subbranchen am meisten gewachsen und welche im Vergleich zur Gesamtschweiz am stärksten vertreten sind.')
                      ),
                      "dumbbell" = list(
                        title = paste0("So haben sich die Beschäftigtenzahlen im Kanton Thurgau in den letzten zehn Jahren (", min_year, "-", max_year,") verändert"),
                        text = paste0('Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Subbranchen des verarbeitenden Gewerbes des Kantons Thurgau im Zehnjahreszeitraum von ', min_year, ' bis ', max_year,'. Die Subbranchen sind nach den Beschäftigtenzahlen im Jahr ', max_year,' geordnet.')
                      ),
                      "bar" = list(
                        title = paste0('Die Grösse der Subbranchen des verarbeitenden Gewerbes im Vergleich mit der Schweiz'),
                        text = paste0('Die blauen Balken zeigen die Subbranchen des verarbeitenden Gewerbes, in denen im Kanton Thurgau verhältnismässig mehr Beschäftigte arbeiten als in der Gesamtschweiz (Standortquotient > 1). Die violetten Balken zeigen die Subbranchen, die im Kanton Thurgau im Vergleich zur Gesamtschweiz schwächer vertreten sind (Standortquotient < 1).')
                      )

    )
    div(
      class = "chart-description",
      h4(content$title),
      HTML(paste0('<p>', content$text, '</p>'))
    )
  })



  # Tab 2 - Bezirke descriptions
  output$dynamic_description_bezirke <- renderUI({
    chart_type <- current_chart_bezirke()
    selected_bezirk <- input$dataset_choice

    # Use first dataset if no selection
    if(is.null(selected_bezirk) || selected_bezirk == "") {
      selected_bezirk <- if(length(datasets) > 0) names(datasets)[1] else ""
    }

    content <- switch(chart_type,
                      "bubble" = list(
                        title = paste0("Die Branchenstruktur im Bezirk ", selected_bezirk),
                        text = paste0('Diese Grafik zeigt, welches die grössten Branchen des Bezirks ', selected_bezirk, ' sind, welche Branchen am meisten gewachsen und welche im Vergleich zur Gesamtschweiz am stärksten vertreten sind.')
                      ),
                      "dumbbell" = list(
                        title = paste0("So haben sich die Beschäftigtenzahlen im Bezirk ", selected_bezirk, " in den letzten zehn Jahren (", min_year, "-", max_year,") verändert"),
                        text = paste0('Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Branchen des ', selected_bezirk,' im Zehnjahreszeitraum von ', min_year, ' bis ', max_year, '. Die Branchen sind nach den Beschäftigtenzahlen im Jahr ', max_year,' geordnet.')
                      ),
                      "bar" = list(
                        title = paste0("Die Grösse der Branchen im Bezirk ", selected_bezirk, " im Vergleich zur Gesamtschweiz"),
                        text = paste0('Die blauen Balken zeigen die Branchen, in denen im Bezirk ', selected_bezirk, ' verhältnismässig mehr Beschäftigte arbeiten als in der Gesamtschweiz (Standortquotient > 1). Die violetten Balken zeigen die Branchen, die im Bezirk ', selected_bezirk, ' im Vergleich zur Gesamtschweiz schwächer vertreten sind (Standortquotient < 1).')
                      )
    )

    div(
      class = "chart-description-select",
      h4(content$title),
      HTML(paste0('<p>', content$text, '</p>'))
    )
  })



  # Tab 3 - Gemeinden descriptions
  output$dynamic_description_gemeinden <- renderUI({
    chart_type <- current_chart_gemeinden()
    selected_gemeinde <- input$dataset_choice_GM

    # Use first dataset if no selection
    if(is.null(selected_gemeinde) || selected_gemeinde == "") {
      selected_gemeinde <- if(length(datasets_GM) > 0) names(datasets_GM)[1] else ""
    }

    content <- switch(chart_type,
                      "bubble" = list(
                        title = paste0("Die Branchenstruktur in der Gemeinde ", selected_gemeinde),
                        text = paste0('Diese Grafik zeigt, welches die grössten Branchen der Gemeinde ', selected_gemeinde, ' sind, welche Branchen am meisten gewachsen und welche im Vergleich zur Gesamtschweiz am stärksten vertreten sind.')
                      ),
                      "dumbbell" = list(
                        title = paste0("So haben sich die Beschäftigtenzahlen in der Gemeinde ", selected_gemeinde, " in den letzten zehn Jahren (", min_year, "-", max_year,") verändert"),
                        text = paste0('Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Branchen der Gemeinde ', selected_gemeinde, ' im Zehnjahreszeitraum von ', min_year, ' bis ', max_year, '. Die Branchen sind nach den Beschäftigtenzahlen im Jahr ', max_year,' geordnet.')
                      ),
                      "bar" = list(
                        title = paste0("Die Grösse der Branchen in der Gemeinde ", selected_gemeinde, " im Vergleich zur Gesamtschweiz"),
                        text = paste0('Die blauen Balken zeigen die Branchen, in denen in der Gemeinde ', selected_gemeinde, ' verhältnismässig mehr Beschäftigte arbeiten als in der Gesamtschweiz (Standortquotient > 1). Die violetten Balken zeigen die Branchen, die in der Gemeinde ', selected_gemeinde, ' im Vergleich zur Gesamtschweiz schwächer vertreten sind (Standortquotient < 1).')
                      )
    )

    div(
      class = "chart-description-select",
      h4(content$title),
      HTML(paste0('<p>', content$text, '</p>'))
    )
  })


  # ========== TAB 1 - KANTON BUTTON OBSERVERS ==========
  observeEvent(input$btn_bubble_tg, {
    current_chart_tg("bubble")
  })

  observeEvent(input$btn_dumbbell_tg, {
    current_chart_tg("dumbbell")
  })

  observeEvent(input$btn_bar_tg, {
    current_chart_tg("bar")
  })

  # ========== VERARBEITENDES GEWERBE BUTTON OBSERVERS (moved from tab 4) ==========
  observeEvent(input$btn_bubble_gewerbe, {
    current_chart_gewerbe("bubble")
  })

  observeEvent(input$btn_dumbbell_gewerbe, {
    current_chart_gewerbe("dumbbell")
  })

  observeEvent(input$btn_bar_gewerbe, {
    current_chart_gewerbe("bar")
  })

  # ========== TAB 2 - BEZIRKE BUTTON OBSERVERS ==========
  observeEvent(input$btn_bubble_bezirke, {
    current_chart_bezirke("bubble")
  })

  observeEvent(input$btn_dumbbell_bezirke, {
    current_chart_bezirke("dumbbell")
  })

  observeEvent(input$btn_bar_bezirke, {
    current_chart_bezirke("bar")
  })


  # ========== TAB 3 - GEMEINDEN BUTTON OBSERVERS ==========
  observeEvent(input$btn_bubble_gemeinden, {
    current_chart_gemeinden("bubble")
  })

  observeEvent(input$btn_dumbbell_gemeinden, {
    current_chart_gemeinden("dumbbell")
  })

  observeEvent(input$btn_bar_gemeinden, {
    current_chart_gemeinden("bar")
  })


  # ========== REACTIVE DATA ==========
  selected_data <- reactive({
    dataset_name <- if(is.null(input$dataset_choice) || input$dataset_choice == "") {
      if(length(datasets) > 0) names(datasets)[1] else NULL
    } else {
      input$dataset_choice
    }

    req(dataset_name)
    if(length(datasets) > 0 && dataset_name %in% names(datasets)) {
      datasets[[dataset_name]]
    } else {
      data.frame()
    }
  })


  selected_data_GM <- reactive({
    dataset_name <- if(is.null(input$dataset_choice_GM) || input$dataset_choice_GM == "") {
      if(length(datasets_GM) > 0) names(datasets_GM)[1] else NULL
    } else {
      input$dataset_choice_GM
    }

    req(dataset_name)
    if(length(datasets_GM) > 0 && dataset_name %in% names(datasets_GM)) {
      datasets_GM[[dataset_name]]
    } else {
      data.frame()
    }
  })


  # ========== DYNAMIC CHART OUTPUTS ==========

  # TAB 1 - KANTON: Dynamic chart output
  output$dynamic_chart_tg <- renderHighchart({
    if(!"Thurgau" %in% names(datasets_TG)) return(NULL)

    data <- datasets_TG[["Thurgau"]]

    switch(current_chart_tg(),
           "bubble" = {

             # Set minimum padding and calculate proportional padding
             x_padding <- 0.1
             y_padding <- 0.5

             # Apply padding to create chart boundaries
             minx <- min(data$x, na.rm = TRUE) - x_padding
             maxx <- max(data$x, na.rm = TRUE) + x_padding
             miny <- min(data$y, na.rm = TRUE) - y_padding
             maxy <- max(data$y, na.rm = TRUE) + y_padding

             create_bubble_chart(
               data = data,
               legend_text = "Die grössten drei Branchen des Kantons Thurgau",
               size = "20%",
               minx = minx,
               maxx = maxx,
               miny = miny,
               maxy = maxy,
               level_type = "kanton",
               max_year = max_year,
               min_year = min_year
             ) %>%
               hc_plotOptions(bubble = list(minSize = "0.3%"))
           },
           "dumbbell" = {
             # Calculate dynamic max value for dumbbell chart
             max_value <- max(pmax(data$current, data$previous, na.rm = TRUE), na.rm = TRUE)

             create_dumbbell_chart(
               data = data,
               max_value = max_value,
               max_year = max_year,
               min_year = min_year
             )
           },
           "bar" = create_bar_chart(
             data = data,
             max_year = max_year)
    )
  })


  # VERARBEITENDES GEWERBE: Dynamic chart output (moved from tab 4)
  output$dynamic_chart_gewerbe <- renderHighchart({
    if(!"Thurgau_nur_C" %in% names(datasets_TG)) return(NULL)

    data <- datasets_TG[["Thurgau_nur_C"]]

    switch(current_chart_gewerbe(),
           "bubble" = {             # Calculate dynamic min/max values for bubble chart
             # Calculate proportional padding for both axes
             x_range <- max(data$x, na.rm = TRUE) - min(data$x, na.rm = TRUE)
             y_range <- max(data$y, na.rm = TRUE) - min(data$y, na.rm = TRUE)

             # Set minimum padding and calculate proportional padding
             x_padding <- max(0.1, x_range * 0.1)  # At least 0.1, or 10% of range
             y_padding <- 2    # At least 2, or 10% of range

             # Apply padding to create chart boundaries
             minx <- min(data$x, na.rm = TRUE) - x_padding
             maxx <- max(data$x, na.rm = TRUE) + x_padding
             miny <- min(data$y, na.rm = TRUE) - y_padding
             maxy <- max(data$y, na.rm = TRUE) + y_padding

             create_bubble_chart(
               data = data,
               legend_text = "Die grössten drei Subbranchen des verarbeitenden Gewerbes",
               size = "20%",
               minx = minx,
               maxx = maxx,
               miny = miny,
               maxy = maxy,
               level_type = "c_sektor",
               max_year = max_year,
               min_year = min_year
             ) %>%
               hc_plotOptions(bubble = list(minSize = "0.3%"))
           },
           "dumbbell" = {
             # Calculate dynamic max value for dumbbell chart
             max_value <- max(pmax(data$current, data$previous, na.rm = TRUE), na.rm = TRUE)

             create_dumbbell_chart(
               data = data,
               max_value = max_value,
               max_year = max_year,
               min_year = min_year
             )
           },
           "bar" = create_bar_chart(
             data = data,
             level_type = "c_sektor",
             max_year = max_year
           )
    )
  })


  # TAB 2 - BEZIRKE: Dynamic chart output
  output$dynamic_chart_bezirke <- renderHighchart({
    data <- selected_data()
    if(nrow(data) == 0) return(NULL)

    # Get dataset name (same logic as level_name)
    dataset_name <- if(is.null(input$dataset_choice) || input$dataset_choice == "") {
      if(length(datasets) > 0) names(datasets)[1] else ""
    } else {
      input$dataset_choice
    }

    switch(current_chart_bezirke(),
           "bubble" = {
             # Calculate dynamic min/max values for bubble chart
             # Calculate proportional padding for both axes
             x_range <- max(data$x, na.rm = TRUE) - min(data$x, na.rm = TRUE)
             y_range <- max(data$y, na.rm = TRUE) - min(data$y, na.rm = TRUE)

             # Set minimum padding and calculate proportional padding
             x_padding <- max(0.1, x_range * 0.1)  # At least 0.1, or 10% of range
             y_padding <- 2    # At least 2, or 10% of range

             # Apply padding to create chart boundaries
             minx <- min(data$x, na.rm = TRUE) - x_padding
             maxx <- max(data$x, na.rm = TRUE) + x_padding
             miny <- min(data$y, na.rm = TRUE) - y_padding
             maxy <- max(data$y, na.rm = TRUE) + y_padding

             create_bubble_chart(
               data = data,
               legend_text = paste0("Die grössten drei Branchen des Bezirks ", dataset_name),
               size = "20%",
               minx = minx,
               maxx = maxx,
               miny = miny,
               maxy = maxy,
               level_type = "bezirk",
               max_year = max_year,
               min_year = min_year,
               level_name = dataset_name
             ) %>%
               hc_plotOptions(bubble = list(minSize = "0.3%"))
           },
           "dumbbell" = {
             # Calculate dynamic max value for dumbbell chart
             max_value <- max(pmax(data$current, data$previous, na.rm = TRUE), na.rm = TRUE)

             create_dumbbell_chart(
               data = data,
               max_value = max_value,
               max_year = max_year,
               min_year = min_year
             )
           },
           "bar" = create_bar_chart(
             data = data,
             max_year = max_year
             )
    )
  })

  # TAB 3 - GEMEINDEN: Dynamic chart output
  output$dynamic_chart_gemeinden <- renderHighchart({
    data <- selected_data_GM()
    if(nrow(data) == 0) return(NULL)

    # Get dataset name (same logic as level_name)
    dataset_name <- if(is.null(input$dataset_choice_GM) || input$dataset_choice_GM == "") {
      if(length(datasets_GM) > 0) names(datasets_GM)[1] else ""
    } else {
      input$dataset_choice_GM
    }

    switch(current_chart_gemeinden(),
           "bubble" = {
             # Calculate dynamic min/max values for bubble chart
             # Calculate proportional padding for both axes
             x_range <- max(data$x, na.rm = TRUE) - min(data$x, na.rm = TRUE)
             y_range <- max(data$y, na.rm = TRUE) - min(data$y, na.rm = TRUE)

             # Set minimum padding and calculate proportional padding
             x_padding <- max(0.1, x_range * 0.1)  # At least 0.1, or 10% of range
             y_padding <- 2    # At least 2, or 10% of range

             # Apply padding to create chart boundaries
             minx <- min(data$x, na.rm = TRUE) - x_padding
             maxx <- max(data$x, na.rm = TRUE) + x_padding
             miny <- min(data$y, na.rm = TRUE) - y_padding
             maxy <- max(data$y, na.rm = TRUE) + y_padding

             create_bubble_chart(
               data = data,
               legend_text = paste0("Die grössten drei Branchen der Gemeinde ", dataset_name),
               size = "20%",
               minx = minx,
               maxx = maxx,
               miny = miny,
               maxy = maxy,
               level_type = "gemeinde",
               max_year = max_year,
               min_year = min_year,
               level_name = dataset_name
             ) %>%
               hc_plotOptions(bubble = list(minSize = "0.3%"))
           },
           "dumbbell" = {
             # Calculate dynamic max value for dumbbell chart
             max_value <- max(pmax(data$current, data$previous, na.rm = TRUE), na.rm = TRUE)

             create_dumbbell_chart(
               data = data,
               max_value = max_value,
               max_year = max_year,
               min_year = min_year
             )
           },
           "bar" = create_bar_chart(
             data = data,
             max_year = max_year)
    )
  })

  # ========== BUTTON STYLE MANAGEMENT ==========

  # Update button styles for Kanton tab
  observe({
    current <- current_chart_tg()

    # Reset all buttons to outline style
    shinyjs::removeClass("btn_bubble_tg", "btn-primary")
    shinyjs::removeClass("btn_dumbbell_tg", "btn-primary")
    shinyjs::removeClass("btn_bar_tg", "btn-primary")
    shinyjs::addClass("btn_bubble_tg", "btn-outline-primary")
    shinyjs::addClass("btn_dumbbell_tg", "btn-outline-primary")
    shinyjs::addClass("btn_bar_tg", "btn-outline-primary")

    # Set active button to primary style
    if(current == "bubble") {
      shinyjs::removeClass("btn_bubble_tg", "btn-outline-primary")
      shinyjs::addClass("btn_bubble_tg", "btn-primary")
    } else if(current == "dumbbell") {
      shinyjs::removeClass("btn_dumbbell_tg", "btn-outline-primary")
      shinyjs::addClass("btn_dumbbell_tg", "btn-primary")
    } else if(current == "bar") {
      shinyjs::removeClass("btn_bar_tg", "btn-outline-primary")
      shinyjs::addClass("btn_bar_tg", "btn-primary")
    }
  })


  # Update button styles for Verarbeitendes Gewerbe (moved from tab 4)
  observe({
    current <- current_chart_gewerbe()

    # Reset all buttons to outline style
    shinyjs::removeClass("btn_bubble_gewerbe", "btn-primary")
    shinyjs::removeClass("btn_dumbbell_gewerbe", "btn-primary")
    shinyjs::removeClass("btn_bar_gewerbe", "btn-primary")
    shinyjs::addClass("btn_bubble_gewerbe", "btn-outline-primary")
    shinyjs::addClass("btn_dumbbell_gewerbe", "btn-outline-primary")
    shinyjs::addClass("btn_bar_gewerbe", "btn-outline-primary")

    # Set active button to primary style
    if(current == "bubble") {
      shinyjs::removeClass("btn_bubble_gewerbe", "btn-outline-primary")
      shinyjs::addClass("btn_bubble_gewerbe", "btn-primary")
    } else if(current == "dumbbell") {
      shinyjs::removeClass("btn_dumbbell_gewerbe", "btn-outline-primary")
      shinyjs::addClass("btn_dumbbell_gewerbe", "btn-primary")
    } else if(current == "bar") {
      shinyjs::removeClass("btn_bar_gewerbe", "btn-outline-primary")
      shinyjs::addClass("btn_bar_gewerbe", "btn-primary")
    }
  })


  # Update button styles for Bezirke tab
  observe({
    current <- current_chart_bezirke()

    # Reset all buttons to outline style
    shinyjs::removeClass("btn_bubble_bezirke", "btn-primary")
    shinyjs::removeClass("btn_dumbbell_bezirke", "btn-primary")
    shinyjs::removeClass("btn_bar_bezirke", "btn-primary")
    shinyjs::addClass("btn_bubble_bezirke", "btn-outline-primary")
    shinyjs::addClass("btn_dumbbell_bezirke", "btn-outline-primary")
    shinyjs::addClass("btn_bar_bezirke", "btn-outline-primary")

    # Set active button to primary style
    if(current == "bubble") {
      shinyjs::removeClass("btn_bubble_bezirke", "btn-outline-primary")
      shinyjs::addClass("btn_bubble_bezirke", "btn-primary")
    } else if(current == "dumbbell") {
      shinyjs::removeClass("btn_dumbbell_bezirke", "btn-outline-primary")
      shinyjs::addClass("btn_dumbbell_bezirke", "btn-primary")
    } else if(current == "bar") {
      shinyjs::removeClass("btn_bar_bezirke", "btn-outline-primary")
      shinyjs::addClass("btn_bar_bezirke", "btn-primary")
    }
  })

  # Update button styles for Gemeinden tab
  observe({
    current <- current_chart_gemeinden()

    # Reset all buttons to outline style
    shinyjs::removeClass("btn_bubble_gemeinden", "btn-primary")
    shinyjs::removeClass("btn_dumbbell_gemeinden", "btn-primary")
    shinyjs::removeClass("btn_bar_gemeinden", "btn-primary")
    shinyjs::addClass("btn_bubble_gemeinden", "btn-outline-primary")
    shinyjs::addClass("btn_dumbbell_gemeinden", "btn-outline-primary")
    shinyjs::addClass("btn_bar_gemeinden", "btn-outline-primary")

    # Set active button to primary style
    if(current == "bubble") {
      shinyjs::removeClass("btn_bubble_gemeinden", "btn-outline-primary")
      shinyjs::addClass("btn_bubble_gemeinden", "btn-primary")
    } else if(current == "dumbbell") {
      shinyjs::removeClass("btn_dumbbell_gemeinden", "btn-outline-primary")
      shinyjs::addClass("btn_dumbbell_gemeinden", "btn-primary")
    } else if(current == "bar") {
      shinyjs::removeClass("btn_bar_gemeinden", "btn-outline-primary")
      shinyjs::addClass("btn_bar_gemeinden", "btn-primary")
    }
  })


  # ========== DOWNLOAD HANDLERS ==========

  output$download_KT <- downloadHandler(
    filename = function() {
      paste("Kanton_Wirtschaftsstruktur_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Copy the file from /data directory to the download location
      file.copy(from = "Daten/daten_kanton.xlsx",
                to = file,
                overwrite = TRUE)
    }
  )

  output$download_gewerbe <- downloadHandler(
    filename = function() {
      paste("Verarbeitendes_Gewerbe_Struktur_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy(from = "Daten/daten_sectorC.xlsx",
                to = file,
                overwrite = TRUE)
    }
  )

  output$download_bezirke <- downloadHandler(
    filename = function() {
      paste("Bezirk_Wirtschaftsstruktur_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy(from = "Daten/daten_bezirk.xlsx",
                to = file,
                overwrite = TRUE)
    }
  )

  output$download_gemeinden <- downloadHandler(
    filename = function() {
      paste("Gemeinde_Wirtschaftsstruktur_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy(from = "Daten/daten_gemeinde.xlsx",
                to = file,
                overwrite = TRUE)
    }
  )


}

# Run app
shinyApp(ui = ui, server = server)
