library(shiny)
library(plotly)

source("ui/ui.R")
source("server/server.R")
source("global.R")

shinyApp(ui, server)
