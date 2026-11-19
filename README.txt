"Daten/02_b_data.R" ausführen um die neuen Daten einzulesen


Anmelden bei Shiny mit
rsconnect::setAccountInfo(name='statistiktg',
                          token= <TOKEN AUS SHINYAPPS>,
                          secret=<SECRET AUS SHINYAPPS>)


In der Console ausführen, um auf shinyapps hochzuladen
rsconnect::deployApp("/r-proj/stat/ogd/branchenstruktur/Beschäftigte nach NOGA nach Gemeinde/App")

rsconnect::deployApp("/r-proj/stat/ogd/branchenstruktur/Beschäftigte nach NOGA nach Gemeinde/Sicherheitskopien/App_05.08.25_V3/")

Lesebeispiel muss JEDES JAHr von hand aktualisiert werden
