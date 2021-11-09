### APP.R ###

# Libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)

# Server and UI Source
source("app_ui.R")
source("app_server.R")


shinyApp(ui, server)