#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @importFrom dplyr arrange desc mutate
#' @importFrom highcharter highchart highchartOutput renderHighchart hc_chart
#'   hc_xAxis hc_yAxis hc_add_series hc_series hc_tooltip hc_credits hc_legend
#'   hc_plotOptions hc_add_annotation list_parse JS
NULL

utils::globalVariables(c("current", "previous", "x", "y", "z", "color", "sinkt"))
