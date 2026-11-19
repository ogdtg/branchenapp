#' Die Shiny-Applikation Server-Logik
#'
#' @param input,output,session Interne Shiny-Parameter.
#' @noRd
app_server <- function(input, output, session) {
  app_data <- load_branchen_data()
  for (level in names(ebenen())) {
    mod_branchen_server(level, level, app_data)
  }
}
