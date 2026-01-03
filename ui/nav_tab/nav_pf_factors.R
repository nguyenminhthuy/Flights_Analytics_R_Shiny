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
                         textOutput("pf_ft_year_text", inline = TRUE),
                         
                         tags$span(" | "),
                         
                         tags$strong("AIRLINES: "),
                         textOutput("pf_ft_airline_text", inline = TRUE),
                         
                         tags$span(" | "),
                         
                         tags$strong("SEASON: "),
                         textOutput("pf_ft_season_text", inline = TRUE),
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
            inputId = "pf_ft_year_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            width = "100%"
          ),
          br(),
          
          # ===== AIRLINE SELECT =====
          tags$label(icon("plane"), " Airline"),
          selectInput(
            inputId = "pf_ft_airline_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          # ===== SEASON  =====
          tags$label(icon("cloud-sun"), " Season"),
          selectInput(
            inputId = "pf_ft_season_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          actionButton(
            inputId = "pf_ft_apply_filter",
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
                         width = 3,
                         div(class = "eda-card",
                             plotlyOutput("pf_ft_influence_of_delays", height = "420px")
                         )
                       ),
                       
                       column(
                         width = 9,
                         div(class = "eda-card",
                             style = "margin-bottom: 20px;",
                             plotlyOutput("pf_ft_delay_factor_interaction", height = "420px")
                         )
                       )
              )
              
          )
        )
      )
  )
)
