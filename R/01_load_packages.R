# Packages required to run the Shiny app itself.
# Packages needed only to (re-)build the data (BFS, tgAPI, openxlsx, TGexcel,
# readxl, stringr) live in Daten/02_b_data.R, which is run separately whenever
# the OGD source data is updated.
library(dplyr)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(highcharter)
library(shinybrowser)

