#' Diagramm-Karte einer Gebietsebene (UI)
#'
#' Karte im tgdashboard-Stil mit den Tabs Übersicht (Bubble), Grösse und
#' Wachstum (Dumbbell) und Standortquotient (Balken). Quellen, Download und
#' weiterführende Links stehen in der Fusszeile.
#'
#' @param id Modul-ID.
#' @param choices Optional: Gebiete für die Auswahl (Bezirke, Gemeinden).
#' @param label Beschriftung der Gebietsauswahl.
#' @param lesehilfe Lesebeispiel unter dem Bubble-Chart anzeigen?
#' @noRd
mod_branchen_ui <- function(id, choices = NULL, label = NULL, lesehilfe = FALSE) {
  ns <- NS(id)
  tagList(
    if (!is.null(choices)) gebiet_auswahl(ns("gebiet"), label, choices),
    bslib::navset_card_tab(
      id = ns("chart"),
      title = uiOutput(ns("titel"), class = "echr-title-text"),
      full_screen = TRUE,
      chart_tab(ns, "bubble", "braille", "Übersicht", "600px", if (lesehilfe) lesehilfe_ui(ns)),
      chart_tab(ns, "dumbbell", "arrows-left-right", "Detail: Grösse und Wachstum", "450px"),
      chart_tab(ns, "bar", "chart-bar", "Detail: Standortquotient", "450px"),
      footer = bslib::card_footer(karten_quellen(ns))
    )
  )
}

#' Suchbare Gebietsauswahl
#' @noRd
gebiet_auswahl <- function(id, label, choices) {
  div(
    class = "gebiet-auswahl",
    shinyWidgets::virtualSelectInput(
      inputId = id,
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
}

#' Tab mit Beschreibung und Diagramm
#' @param ns Namespace-Funktion.
#' @param chart `bubble`, `dumbbell` oder `bar` (Tab-Wert und Output-ID).
#' @param icon,tooltip Tab-Symbol und Tooltip.
#' @param height Diagrammhöhe.
#' @param ... Weitere UI-Elemente unter dem Diagramm.
#' @noRd
chart_tab <- function(ns, chart, icon, tooltip, height, ...) {
  bslib::nav_panel(
    title = bslib::tooltip(shiny::icon(icon), tooltip, options = list(delay = list(show = 500, hide = 0))),
    value = chart,
    div(class = "echr-subtitle-text chart-text", textOutput(ns(paste0("text_", chart)), inline = TRUE)),
    mit_spinner(highchartOutput(ns(chart), height = height)),
    ...
  )
}

#' Aufklappbare Lesehilfe
#' @noRd
lesehilfe_ui <- function(ns) {
  bslib::accordion(
    class = "plot-supl-accordion", open = FALSE,
    bslib::accordion_panel("Lesehilfe", uiOutput(ns("lesehilfe")))
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

    gebiet <- reactive({
      if (length(daten) == 1) names(daten) else req(input$gebiet)
    })
    data <- reactive({
      req(gebiet() %in% names(daten))
      daten[[gebiet()]]
    })
    texte <- reactive(chart_texte(level, gebiet(), min_year, max_year))

    output$titel <- renderUI(texte()[[if (is.null(input$chart)) "bubble" else input$chart]]$title)
    output$text_bubble <- renderText(texte()$bubble$text)
    output$text_dumbbell <- renderText(texte()$dumbbell$text)
    output$text_bar <- renderText(texte()$bar$text)

    output$bubble <- renderHighchart({
      create_bubble_chart(data(), level, max_year, min_year, top3_text(level, gebiet()))
    })
    output$dumbbell <- renderHighchart(create_dumbbell_chart(data(), max_year, min_year))
    output$bar <- renderHighchart(create_bar_chart(data(), level, max_year))
    output$lesehilfe <- renderUI(render_lesebeispiel(data(), max_year, min_year))
    output$hinweis <- renderUI(hinweis_referenzjahr(app_data))

    output$download <- downloadHandler(
      filename = function() paste0(ebenen()[[level]]$download, "_", max_year, ".xlsx"),
      content = function(file) {
        writexl::write_xlsx(list(Daten = downloadtabelle(app_data$tabellen[[level]], level)), file)
      }
    )
  })
}
