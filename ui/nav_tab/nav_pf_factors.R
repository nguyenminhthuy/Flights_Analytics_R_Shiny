nav_pf_factors <- tagList(
  
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
                         tags$h4("FACTORS"),
                         tags$hr(class = "hr-main"),
                         
                         tags$strong("YEARS: "),
                         tags$span("2020–2023"),
                         
                         tags$span(" | "),
                         
                         tags$strong("AIRLINES: "),
                         tags$span("Delta, United"),
                         
                         tags$span(" | "),
                         
                         tags$strong("SEASON: "),
                         tags$span("Delta, United")
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("?"),
                         tags$strong("Flights")
                         
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("?"),
                         tags$strong("Avg Dep")
                     )
                   ),
                   column(
                     width = 2,
                     div(class = "eda-card eda-card-text",
                         tags$h3("?"),
                         tags$strong("Avg Arr")
                         
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
            inputId = "year_select",
            label = NULL,
            choices = c("All", "2019", "2020", "2021", "2022", "2023"),
            selected = "All",
            width = "100%"
          ),
          br(),
          
          # ===== AIRLINE SELECT =====
          tags$label(icon("plane"), " Airline"),
          selectInput(
            inputId = "airline_select",
            label = NULL,
            choices = c("All", "Delta", "United", "American", "Southwest", "CA", "DD"),
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          # ===== SEASON  =====
          tags$label(icon("cloud-sun"), " Season"),
          selectInput(
            inputId = "season_select",
            label = NULL,
            choices = c("All", "Delta", "United", "American", "Southwest", "CA", "DD"),
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          actionButton(
            inputId = "apply_filter",
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
                             h3("Chart: INFLUENCE OF VARIOUS DELAYS"),
                             p("Nội dung cột 1-2")
                         )
                       ),
                       
                       column(
                         width = 6,
                         div(class = "eda-card",
                             style = "margin-bottom: 20px;",
                             h3("Chart: Marginal Effect of Delay Components on Arrival Delay"),
                             p("Nội dung cột 1-2")
                         ),
                         div(class = "eda-card",
                             h3("Chart: Reliability vs Performance of Delay Factors"),
                             p("Nội dung cột 1-2")
                         )
                       )
              )
              
          )
        )
      )
  )
)
