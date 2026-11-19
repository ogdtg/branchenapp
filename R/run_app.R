#' Branchenapp starten
#'
#' @param onStart,options,enableBookmarking,uiPattern Siehe [shiny::shinyApp()].
#' @param ... Weitere Argumente, abrufbar via [golem::get_golem_options()].
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Daten beim Prozessstart laden, damit der erste Besuch nicht wartet
  load_branchen_data()
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
