#' Die Shiny-Applikation UI
#'
#' @param request Interner Shiny-Parameter.
#' @noRd
app_ui <- function(request) {
  app_data <- load_branchen_data()

  tagList(
    golem_add_external_resources(),
    tgdashboard::tg_header(
      title = "Branchenstruktur",
      subtitle = paste0(
        "Beschäftigte nach Branchen im Kanton Thurgau: Grösse, Wachstum ",
        app_data$min_year, "-", app_data$max_year, " und Standortquotient"
      )
    ),
    bslib::page_fixed(
      title = "Branchenstruktur Kanton Thurgau",
      theme = bslib::bs_theme(brand = system.file("_brand.yml", package = "tgdashboard")),
      tgdashboard::chart_title_inject_css(),
      div(
        class = "full-bleed",
        bslib::navset_tab(
          id = "ebene",
          bslib::nav_panel(
            "Kanton",
            mod_branchen_ui("kanton", lesehilfe = TRUE),
            h3("Zoom auf das verarbeitende Gewerbe", class = "zoom-titel"),
            mod_branchen_ui("c_sektor")
          ),
          bslib::nav_panel(
            "Bezirke",
            mod_branchen_ui("bezirk", choices = names(app_data$daten$bezirk), label = "Bezirk")
          ),
          bslib::nav_panel(
            "Gemeinden",
            mod_branchen_ui("gemeinde", choices = names(app_data$daten$gemeinde), label = "Gemeinde")
          )
        )
      )
    ),
    tgdashboard::tg_footer(
      "ftr",
      address_org = "Amt für Daten und Statistik Thurgau",
      contact_email = "statistik@tg.ch",
      address_street = "Grabenstrasse 8",
      address_city = "8510 Frauenfeld"
    )
  )
}

#' Externe Ressourcen (Favicon, inst/app/www/*.css)
#'
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "Branchenstruktur Kanton Thurgau")
  )
}
