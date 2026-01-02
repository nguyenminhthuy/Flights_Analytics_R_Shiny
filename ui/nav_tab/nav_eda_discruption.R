nav_eda_discruption <- tagList(
  
  # LOAD CSS
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  div(class = "bslib-page-main page-bg",
      # -------- Selected choices --------
      div(class = "container-fluid",
          fluidRow(style = "margin-bottom: 20px;",
                   column(
                     width = 6,
                     div(class = "eda-card eda-card-text",
                         tags$h4("Flight Disruptions (Cancellations & Diversions)"),
                         tags$hr(class = "hr-main"),
                         
                         tags$strong("YEARS: "),
                         textOutput("dis_year_text", inline = TRUE),
                         
                         tags$span(" | "),
                         
                         tags$strong("AIRLINES: "),
                         textOutput("dis_airline_text", inline = TRUE),
                         
                         tags$span(" | "),
                         
                         tags$strong("SEASON: "),
                         textOutput("dis_season_text", inline = TRUE),
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("2.72%"),
                         tags$strong("Cancelled flights")
                         
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("0.2%"),
                         tags$strong("Diverted flights")
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("732K"),
                         tags$strong("Total Disrupted Flights")
                         
                     )
                   ),
          )
      ),
      
      sidebarLayout(
        # -------- Sidebar --------
        sidebarPanel(
          width = 3,
          tags$h3("Data Filter"),
          tags$hr(class = "hr-main"),
          
          # ===== YEAR SELECT =====
          tags$label(icon("calendar-days"), " Year"),
          selectInput(
            inputId = "dis_year_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            width = "100%"
          ),
          br(),
          
          # ===== AIRLINE SELECT =====
          tags$label(icon("plane"), " Airline"),
          selectInput(
            inputId = "dis_airline_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          # ===== SEASON  =====
          tags$label(icon("cloud-sun"), " Season"),
          selectInput(
            inputId = "dis_season_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          actionButton(
            inputId = "dis_apply_filter",
            label = "Apply Filter",
            icon = icon("filter"),
            class = "btn-apply-filter"
          ),
          
          br(), br()
        ),
        
        # -------- Main Panel --------
        mainPanel(
          width = 9,
          div(class = "container-fluid",
              fluidRow(style = "margin-bottom: 20px;",
                       column(
                         width = 6,
                         div(class = "eda-card",
                             style = "margin-bottom: 20px;",
                             h3("Chart: Flight Disruption Rate Over Time (Cancelled vs Diverted)"),
                             p("Nội dung cột 1-2")
                         ),
                         div(class = "eda-card",
                             h3("Chart: Share of Disruption Causes"),
                             p("Nội dung cột 1-2")
                         )
                       ),
                       column(
                         width = 6,
                         div(class = "eda-card",
                             h3("?"),
                             p("Nội dung cột 1-2")
                         )
                       )
              ),
              fluidRow(style = "margin-bottom: 20px;",
                       column(
                         width = 12,
                         div(class = "eda-card",
                             h3("?"),
                             p("Nội dung cột 1-2")
                         )
                       )
              )
          )
        )
      )
  )
)
