# Explicit library calls allow deployment tools to discover dependencies.
# Install dependencies during environment setup, never during app startup.
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(gt)
  library(ggplot2)
  library(plotly)
  library(httr2)
  library(jsonlite)
  library(readr)
  library(Kendall)
  library(shinymanager)
})