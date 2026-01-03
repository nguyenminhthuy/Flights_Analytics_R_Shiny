nav_pf_overview <- tagList(
  
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
                  tags$h4("ON-TIME PERFORMANCE & DELAYS"),
                  tags$hr(class = "hr-main"),
                  
                  tags$strong("YEARS: "),
                  textOutput("pf_ov_year_text", inline = TRUE),
                  tags$span(" | "),
                  
                  tags$strong("AIRLINES: "),
                  textOutput("pf_ov_airline_text", inline = TRUE),
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3(textOutput("pf_ov_total_flights")),
                  tags$strong("Flights")
                  
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3(textOutput("pf_ov_on_time_rate")),
                  tags$strong("On-time Rate")
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3(textOutput("pf_ov_delay_rate")),
                  tags$strong("Delay Rate")
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
            inputId = "pf_ov_year_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            width = "100%"
          ),
          br(),
          
          # ===== AIRLINE SELECT =====
          tags$label(icon("plane"), " Airline"),
          selectInput(
            inputId = "pf_ov_airline_select",
            label = NULL,
            choices = NULL,
            selected = "All",
            selectize = TRUE,   # 👈 search enabled
            width = "100%"
          ),
          
          actionButton(
            inputId = "pf_ov_apply_filter",
            label = "Apply Filter",
            icon = icon("filter"),
            class = "btn-apply-filter"
          ),
          
          br(), br()
        ),
        
        # -------- Main Panel --------
        mainPanel(
          width = 9,
          div(class = "eda-card",
              style = "margin-bottom: 20px;",
              plotlyOutput("pf_ov_monthly_volume_delay", height = "350")
          ),
          div(class = "eda-card",
              style = "margin-bottom: 20px;",
              plotlyOutput("pf_ov_delay_causes", height = "300")

          )
        )
      )
  )
)
