# App im Entwicklungsmodus starten
options(golem.app.prod = FALSE)
golem::detach_all_attached()
golem::document_and_reload()
run_app()
