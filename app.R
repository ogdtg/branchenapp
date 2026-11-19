# Startpunkt für Posit Connect / shinyapps.io (golem::add_positconnect_file()).
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
branchenapp::run_app()
