source("ui/nav_tab/nav_eda_overview.R")
source("ui/nav_tab/nav_eda_performance.R")
source("ui/nav_tab/nav_eda_discruption.R")
source("ui/nav_tab/nav_model.R")
source("ui/nav_tab/nav_about.R")

ui <- navbarPage(
  title = "U.S. Flight Operations",
  
  # LOAD CSS
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # ===== EDA =====
  navbarMenu(
    title = "EDA",
    tabPanel("Overview", nav_eda_overview),
    tabPanel("Performance", nav_eda_performance),
    tabPanel("Disruption", nav_eda_discruption)
  ),
  
  # ===== MODEL =====
  navbarMenu(
    title = "Model",
    tabPanel("Model 1", nav_model),
    tabPanel("Model 2", "Coming soon"),
    tabPanel("Model 3", "Coming soon")
  ),
  
  # ===== ABOUT =====
  tabPanel("About", nav_about)
)
