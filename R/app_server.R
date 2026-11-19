#' Die Shiny-Applikation Server-Logik
#'
#' @param input,output,session Interne Shiny-Parameter.
#' @noRd
app_server <- function(input, output, session) {
  app_data <- load_branchen_data()
  lapply(names(ebenen), function(level) mod_branchen_server(level, level, app_data))
  invisible(NULL)
}
