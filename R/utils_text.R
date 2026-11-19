#' Zahl im Schweizer Format, z.B. 1234.5 -> "1'234,5"
#' @noRd
format_ch_number <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = "'", decimal.mark = ",")
}

#' @noRd
capitalize_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Texte aller drei Diagramme einer Ebene
#' @noRd
chart_texte <- function(level, gebiet, min_year, max_year) {
  sapply(c("bubble", "dumbbell", "bar"), chart_text, level = level, gebiet = gebiet,
         min_year = min_year, max_year = max_year, simplify = FALSE)
}

#' Beschreibung zum Standortquotienten-Balkendiagramm
#' @param branchen Bezeichnung der Branchen im Satz.
#' @param wo Ortsangabe, z. B. "im Bezirk Arbon".
#' @noRd
sq_text <- function(level, branchen, wo) {
  farben <- if (level == "c_sektor") c("grünen", "orangen") else c("blauen", "violetten")
  paste0(
    "Die ", farben[1], " Balken zeigen ", branchen, ", in denen ", wo,
    " verhältnismässig mehr Beschäftigte arbeiten als in der Gesamtschweiz (Standortquotient > 1). ",
    "Die ", farben[2], " Balken zeigen ", branchen, ", die ", wo,
    " im Vergleich zur Gesamtschweiz schwächer vertreten sind (Standortquotient < 1)."
  )
}

#' Titel und Beschreibung pro Diagramm
#'
#' @param chart `bubble`, `dumbbell` oder `bar`.
#' @param level Ebene (`kanton`, `c_sektor`, `bezirk`, `gemeinde`).
#' @param gebiet Name des Bezirks bzw. der Gemeinde.
#' @return Liste mit `title` und `text` (`text` kann `NULL` sein).
#' @noRd
chart_text <- function(chart, level, gebiet, min_year, max_year) {
  zeitraum <- paste0(min_year, "-", max_year)
  ordnung <- paste0(" im Zehnjahreszeitraum von ", min_year, " bis ", max_year, ". ")
  if (level == "kanton") {
    switch(chart,
      bubble = list(title = "Die Branchenstruktur im Kanton Thurgau", text = NULL),
      dumbbell = list(
        title = paste0("So haben sich die Beschäftigtenzahlen im Kanton Thurgau in den letzten zehn Jahren (", zeitraum, ") verändert"),
        text = paste0("Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Branchen des Kantons Thurgau",
                      ordnung, "Die Branchen sind nach den Beschäftigtenzahlen im Jahr ", max_year, " geordnet.")
      ),
      bar = list(
        title = "Die Grösse der Thurgauer Branchen im Vergleich zur Gesamtschweiz",
        text = sq_text(level, "Branchen", "im Kanton Thurgau")
      )
    )
  } else if (level == "c_sektor") {
    switch(chart,
      bubble = list(
        title = "Das verarbeitende Gewerbe ist die grösste Branche im Kanton Thurgau",
        text = paste0("Das verarbeitende Gewerbe ist die beschäftigungsstärkste Branche im Kanton Thurgau. ",
                      "Diese Grafik zeigt, welches innerhalb des verarbeitenden Gewerbes die grössten Subbranchen sind, ",
                      "welche Subbranchen am meisten gewachsen und welche im Vergleich zur Gesamtschweiz am stärksten vertreten sind.")
      ),
      dumbbell = list(
        title = paste0("So haben sich die Beschäftigtenzahlen im verarbeitenden Gewerbe in den letzten zehn Jahren (", zeitraum, ") verändert"),
        text = paste0("Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Subbranchen des verarbeitenden Gewerbes des Kantons Thurgau",
                      ordnung, "Die Subbranchen sind nach den Beschäftigtenzahlen im Jahr ", max_year, " geordnet.")
      ),
      bar = list(
        title = "Die Grösse der Subbranchen des verarbeitenden Gewerbes im Vergleich mit der Schweiz",
        text = sq_text(level, "die Subbranchen des verarbeitenden Gewerbes", "im Kanton Thurgau")
      )
    )
  } else {
    wo <- if (level == "bezirk") paste("im Bezirk", gebiet) else paste("in der Gemeinde", gebiet)
    des <- if (level == "bezirk") paste("des Bezirks", gebiet) else paste("der Gemeinde", gebiet)
    switch(chart,
      bubble = list(
        title = paste("Die Branchenstruktur", wo),
        text = paste0("Diese Grafik zeigt, welches die grössten Branchen ", des, " sind, welche Branchen am meisten ",
                      "gewachsen und welche im Vergleich zur Gesamtschweiz am stärksten vertreten sind.")
      ),
      dumbbell = list(
        title = paste0("So haben sich die Beschäftigtenzahlen ", wo, " in den letzten zehn Jahren (", zeitraum, ") verändert"),
        text = paste0("Die Grafik zeigt die Entwicklung der Beschäftigtenzahlen in den verschiedenen Branchen ", des,
                      ordnung, "Die Branchen sind nach den Beschäftigtenzahlen im Jahr ", max_year, " geordnet.")
      ),
      bar = list(
        title = paste0("Die Grösse der Branchen ", wo, " im Vergleich zur Gesamtschweiz"),
        text = sq_text(level, "die Branchen", wo)
      )
    )
  }
}

#' Legendentext für die drei grössten Branchen im Bubble-Chart
#' @noRd
top3_text <- function(level, gebiet) {
  switch(level,
    kanton = "Die grössten drei Branchen des Kantons Thurgau",
    c_sektor = "Die grössten drei Subbranchen des verarbeitenden Gewerbes",
    bezirk = paste("Die grössten drei Branchen des Bezirks", gebiet),
    gemeinde = paste("Die grössten drei Branchen der Gemeinde", gebiet)
  )
}

#' Lesehilfe zum Bubble-Chart auf Kantonsebene
#'
#' Die Zahlen im Lesebeispiel werden aus den Daten berechnet, damit der Text
#' bei jeder Datenaktualisierung stimmt.
#'
#' @param data Kantonsdatensatz aus [prepare_chart_data()].
#' @param branche Branche, die im Lesebeispiel beschrieben wird.
#' @noRd
render_lesebeispiel <- function(data, max_year, min_year,
                                branche = "Gesundheits- und Sozialwesen") {
  top <- data[data$name == branche, ]
  if (nrow(top) != 1) top <- data[which.max(data$z), ]

  wachstum <- if (top$y == 0) "gleich geblieben" else if (top$y > 0) "stark gewachsen" else "zurückgegangen"
  vertretung <- if (top$x == 1) "genau durchschnittlich" else if (top$x > 1) "überdurchschnittlich" else "unterdurchschnittlich"
  rang <- rank(-data$z)[match(top$name, data$name)]
  top3 <- if (rang <= 3) {
    " Sie gehört zu den drei grössten Branchen im Thurgau (grosser Bubble, schwarz umrandet)."
  } else ""

  tagList(
    p("Die Visualisierung zeigt,"),
    tags$ul(
      tags$li("welches die ", strong("grössten"), " Branchen im Kanton Thurgau sind (Je grösser der Bubble, ",
              "desto mehr Beschäftigte arbeiten in der Branche. Die drei grössten Branchen sind schwarz umrandet.)"),
      tags$li("welches die ", strong("wachstumsstärksten"), " Branchen im Kanton Thurgau sind (Je weiter oben der ",
              "Bubble, desto stärker ist die Branche in den letzten Jahren gewachsen.)"),
      tags$li("welches die Branchen sind, die im Kanton Thurgau ", strong("vergleichsweise stark vertreten"),
              " sind (Je weiter rechts der Bubble, desto stärker ist die Branche im Vergleich zur Schweiz vertreten. ",
              "Ein Standortquotient von über 1 bedeutet: In dieser Branche arbeiten im Kanton Thurgau ",
              "verhältnismässig mehr Beschäftigte als in der Gesamtschweiz.)")
    ),
    p("Im ", strong("oberen rechten Quadranten"), " sind die ", strong("Wachstumsbranchen"),
      " dargestellt, die im Kanton Thurgau im Vergleich zur Gesamtschweiz ", strong("stärker vertreten"), " sind."),
    p(
      strong("Lesebeispiel:"),
      paste0(
        " Die Branche «", capitalize_first(top$name), "» im Kanton Thurgau hatte im Jahr ", max_year, " ",
        format_ch_number(top$current), " Beschäftigte.", top3, " Sie ist in den letzten Jahren ", wachstum,
        " (", if (top$y > 0) "+" else "", format_ch_number(top$y, 1), " % im Schnitt der Jahre ",
        min_year, "-", max_year, "). Im Vergleich zur Gesamtschweiz arbeiten im Thurgau ", vertretung,
        " viele Beschäftigte in dieser Branche (Standortquotient von ", format_ch_number(top$x, 2), ")."
      )
    )
  )
}
