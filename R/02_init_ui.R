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
