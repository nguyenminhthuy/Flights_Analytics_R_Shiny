source("ui/nav_tab/nav_eda_overview.R")

source("ui/nav_tab/nav_pf_overview.R")
source("ui/nav_tab/nav_pf_localpatterns.R")
source("ui/nav_tab/nav_pf_factors.R")

source("ui/nav_tab/nav_eda_discruption.R")
source("ui/nav_tab/nav_model1.R")

source("ui/nav_tab/nav_about.R")

source("ui/nav_tab/nav_template.R")

ui <- navbarPage(
  title = "U.S. Flight Operations",
  # LOAD CSS
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # ===== HOME / DATASET OVERVIEW =====
  tabPanel(NULL, # ko để text
    nav_eda_overview, 
    icon = icon("home")
  ),
  
  # ===== PERFORMANCE =====
  navbarMenu(
    title = "Performance",
    tabPanel("Overview", nav_pf_overview),
    tabPanel("Local Patterns", nav_pf_localpatterns),
    tabPanel("Factors", nav_pf_factors)
  ),
  
  # ===== DISCRUPTION =====
  tabPanel("Disruption", nav_eda_discruption),
  
  # ===== MODEL =====
  navbarMenu(
    title = "Model",
    tabPanel("Model 1", nav_model1),
    tabPanel("Model 2", "Coming soon"),
    tabPanel("Model 3", "Coming soon")
  ),
  
  # ===== ABOUT =====
  tabPanel("About", nav_about),
  
  # ===== ABOUT =====
  tabPanel("Template", nav_template)
)
