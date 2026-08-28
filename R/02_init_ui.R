# Formats a number in Swiss/German style, e.g. 1234.5 -> "1'234,5"
format_ch_number <- function(x, digits = 0) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = "'", decimal.mark = ",")
}

capitalize_first <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

# Builds the "Lesebeispiel" text for the Kanton bubble chart based on the
# actual largest branch in `data`, so the text stays correct every year
# without manual edits (data must have columns name, current, x, y, z).
render_lesebeispiel_tg <- function(data, max_year, min_year) {
  # top <- data[order(-data$z), ][1, ]
  top <- data |>
    filter(name == "Gesundheits- und Sozialwesen")

  growth_direction <- if (top$y == 0) "gleich geblieben" else if (top$y > 0) "stark gewachsen" else "zurückgegangen"
  vertretung <- if (top$x == 1) "genau durchschnittlich" else if (top$x > 1) "überdurchschnittlich" else "unterdurchschnittlich"
  growth_sign <- if (top$y > 0) "+" else ""

  HTML(paste0(
    '<p>Die Visualisierung zeigt, </p><ul><li>welches die <strong>grössten</strong> Branchen im Kanton Thurgau sind (Je grösser der Bubble, desto mehr Beschäftigte arbeiten in der Branche. Die drei grössten Branchen sind schwarz umrandet.)</li>
        <li>welches die <strong>wachstumsstärksten</strong>  Branchen im Kanton Thurgau sind (Je weiter oben der Bubble, desto stärker ist die Branche in den letzten Jahren gewachsen.)</li>
        <li>welches die Branchen sind, die im Kanton Thurgau <strong>vergleichsweise stark vertreten</strong> sind (Je weiter rechts der Bubble, desto stärker ist die Branche im Vergleich zur Schweiz vertreten. Ein Standortquotienten von über 1 bedeutet: In dieser Branche arbeiten im Kanton Thurgau verhältnismässig mehr Beschäftigte als in der Gesamtschweiz).</li></ul>
        <p class="secondp">Im <strong>oberen rechten Quadranten</strong> sind die <strong>Wachstumsbranchen</strong> dargestellt, die im Kanton Thurgau im Vergleich zur Gesamtschweiz <strong>stärker vertreten</strong> sind.</p><br>
        <p><strong>Lesebeispiel:</strong> Die Branche «', capitalize_first(top$name), '» im Kanton Thurgau hatte im Jahr ', max_year, ' ', format_ch_number(top$current), ' Beschäftigte. Sie gehört zu den drei grössten Branchen im Thurgau (grosser Bubble, schwarz umrandet). Sie ist in den letzten Jahren ', growth_direction, ' (', growth_sign, format_ch_number(top$y, 1), ' % im Schnitt der Jahre ', min_year, '-', max_year, '). Im Vergleich zur Gesamtschweiz arbeiten im Thurgau ', vertretung, ' viele Beschäftigte in dieser Branche (Standortquotient von ', format_ch_number(top$x, 2), ').</p>'
  ))
}

init_header <- function(dashboard_title, reference='https://statistik.tg.ch'){
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(
      title = dashboard_title,
      href = reference,
    ),
    tags$li(
      a(
        href = reference,
        img(
          src = 'https://www.tg.ch/public/upload/assets/20/logo-kanton-thurgau.svg',
          title = "Company Home",
          height = "30px",
          class = "logoTg"
        ),
        style = "padding-top:10px; padding-bottom:10px;"
      ),
      class = "dropdown"
    )
  )
}

init_navbar <- function(navbar_content){
  bs4Dash::dashboardHeader(
    navbar_content
  )
}


init_body <- function(db_content, navbar_content = NULL){
  bs4Dash::dashboardBody(
    # Add navbar at the top of body (only if provided)
    if(!is.null(navbar_content)) {
      tags$div(class = "custom-navbar-container",
               navbar_content
      )
    },
    useShinyjs(),
    shinybrowser::detect(),
    tags$head(
      includeCSS("www/dashboard_style.css")
    ),
    HTML('<script src="https://cdn.jsdelivr.net/npm/js-cookie@rc/dist/js.cookie.min.js"></script>'),
    tags$script(HTML(
      '
      $(document).on("shiny:connected", function(){
        var newUser = Cookies.get("new_user");
        if(newUser === "false") return;
        Shiny.setInputValue("new_user", true);
        Cookies.set("new_user", false);
      });
      $(document).on("click", ".clickable-element", function() {
        var clicked_id = $(this).attr("id");
        Shiny.setInputValue("clicked_element_id", clicked_id, {priority: "event"});
      });
      $("body").addClass("fixed");
      // Add active state management for navbar
      $(document).on("click", ".navbar-nav .nav-link", function() {
        // Remove active class from all navbar links
        $(".navbar-nav .nav-link").removeClass("active-tab");
        // Add active class to clicked link
        $(this).addClass("active-tab");
      });

      // Header constraint fix
      $(document).ready(function() {
        // Wait for header to be fully rendered
        setTimeout(function() {
          // Find the main header
          var $mainHeader = $(".main-header");

          if ($mainHeader.length > 0) {
            // Create a constraint wrapper if it doesn\'t exist
            if (!$mainHeader.find(".header-constraint").length) {
              // Wrap all header content in a constraint div
              var $headerContent = $mainHeader.children();
              var $constraintDiv = $("<div class=\'header-constraint\'></div>").css({
                "max-width": "1200px",
                "margin": "0 auto",
                "padding": "0 20px",
                "position": "relative",
                "width": "100%",
                "box-sizing": "border-box"
              });

              // Move all content into the constraint wrapper
              $headerContent.appendTo($constraintDiv);
              $constraintDiv.appendTo($mainHeader);

              // Adjust logo positioning
              $(".main-header img.logoTg").css({
                "position": "absolute",
                "right": "20px",
                "top": "13px",
                "z-index": "1031"
              });

              // Adjust dropdown positioning
              $(".main-header .dropdown").css({
                "position": "absolute",
                "right": "80px",
                "top": "13px",
                "z-index": "1031"
              });

              console.log("Header constraint applied successfully");
            }
          }
        }, 200);
      });
      '
    )),
    db_content
  )
}
