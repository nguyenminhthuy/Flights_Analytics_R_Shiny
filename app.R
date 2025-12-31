library(shiny)
library(plotly)
library(leaflet)

source("ui/ui.R")
source("server/server.R")
source("global.R")

shinyApp(ui, server)
