#' Diagramm-Karte einer Gebietsebene (UI)
#'
#' Karte im tgdashboard-Stil mit den Tabs Übersicht (Bubble), Grösse und
#' Wachstum (Dumbbell), Standortquotient (Balken) sowie Daten & Quellen.
#'
#' @param id Modul-ID.
#' @param choices Optional: Gebiete für die Auswahl (Bezirke, Gemeinden).
#' @param label Beschriftung der Gebietsauswahl.
#' @param lesehilfe Lesebeispiel unter dem Bubble-Chart anzeigen?
#' @noRd
mod_branchen_ui <- function(id, choices = NULL, label = NULL, lesehilfe = FALSE) {
  ns <- NS(id)

  tab <- function(icon, tooltip, value, ...) {
    bslib::nav_panel(
      title = bslib::tooltip(shiny::icon(icon), tooltip, options = list(delay = list(show = 500, hide = 0))),
      value = value,
      ...
    )
  }
  chart_panel <- function(value, height, ...) {
    tagList(
      div(class = "echr-subtitle-text chart-text", textOutput(ns(paste0("text_", value)), inline = TRUE)),
      highchartOutput(ns(value), height = height),
      ...
    )
  }

  tagList(
    if (!is.null(choices)) {
      div(
        class = "gebiet-auswahl",
        shinyWidgets::virtualSelectInput(
          inputId = ns("gebiet"),
          label = label,
          choices = choices,
          selected = choices[1],
          search = TRUE,
          width = "100%",
          dropboxWrapper = "body",
          optionsCount = 7,
          noOptionsText = "Keine Optionen",
          noSearchResultsText = "Keine Ergebnisse",
          searchPlaceholderText = "Suchen..."
        )
      )
    },
    bslib::navset_card_tab(
      id = ns("chart"),
      title = uiOutput(ns("titel"), class = "echr-title-text"),
      full_screen = TRUE,
      tab("braille", "Übersicht", "bubble", chart_panel(
        "bubble", "600px",
        if (lesehilfe) {
          bslib::accordion(
            class = "plot-supl-accordion", open = FALSE,
            bslib::accordion_panel("Lesehilfe", uiOutput(ns("lesehilfe")))
          )
        }
      )),
      tab("arrows-left-right", "Detail: Grösse und Wachstum", "dumbbell", chart_panel("dumbbell", "450px")),
      tab("chart-bar", "Detail: Standortquotient", "bar", chart_panel("bar", "450px")),
      tab(
        "info-circle", "Daten und Quellen", "info",
        p("Datenquelle: Bundesamt für Statistik, Statistik der Unternehmensstruktur (STATENT)."),
        div(
          class = "d-flex flex-wrap gap-2",
          downloadButton(ns("download"), "Daten herunterladen (Excel)", class = "btn-primary"),
          link_button("Daten als OGD", "https://data.tg.ch/explore/dataset/sk-stat-150/information/"),
          link_button("Webartikel zum Thema", url_webartikel)
        )
      )
    )
  )
}

#' Diagramm-Karte einer Gebietsebene (Server)
#'
#' @param id Modul-ID.
#' @param level Ebene (`kanton`, `c_sektor`, `bezirk`, `gemeinde`).
#' @param app_data Rückgabe von [load_branchen_data()].
#' @noRd
mod_branchen_server <- function(id, level, app_data) {
  moduleServer(id, function(input, output, session) {
    daten <- app_data$daten[[level]]
    max_year <- app_data$max_year
    min_year <- app_data$min_year
    cfg <- ebenen[[level]]

    gebiet <- reactive({
      if (length(daten) == 1) names(daten) else req(input$gebiet)
    })
    data <- reactive({
      req(gebiet() %in% names(daten))
      daten[[gebiet()]]
    })

    # Letztes angezeigtes Diagramm (für den Titel im Info-Tab)
    aktiv <- reactiveVal("bubble")
    observeEvent(input$chart, {
      if (input$chart != "info") aktiv(input$chart)
    })

    texte <- reactive({
      sapply(c("bubble", "dumbbell", "bar"), chart_text,
             level = level, gebiet = gebiet(),
             min_year = min_year, max_year = max_year, simplify = FALSE)
    })

    output$titel <- renderUI(texte()[[aktiv()]]$title)
    output$text_bubble <- renderText(texte()$bubble$text)
    output$text_dumbbell <- renderText(texte()$dumbbell$text)
    output$text_bar <- renderText(texte()$bar$text)

    output$bubble <- renderHighchart({
      create_bubble_chart(data(), level, max_year, min_year, top3_text(level, gebiet()))
    })
    output$dumbbell <- renderHighchart(create_dumbbell_chart(data(), max_year, min_year))
    output$bar <- renderHighchart(create_bar_chart(data(), level, max_year))

    output$lesehilfe <- renderUI(render_lesebeispiel(data(), max_year, min_year))

    output$download <- downloadHandler(
      filename = function() paste0(cfg$download, "_", Sys.Date(), ".xlsx"),
      content = function(file) file.copy(app_sys("extdata", cfg$xlsx), file, overwrite = TRUE)
    )
  })
}
