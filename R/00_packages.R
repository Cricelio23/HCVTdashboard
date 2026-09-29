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

# Genera manifest.json
# Para publicar una aplicación Shiny en R desde GitHub, Connect Cloud necesita conocer sus dependencias. 
# Desde la raíz del proyecto ejecuta: rsconnect::writeManifest(appDir = ".")
