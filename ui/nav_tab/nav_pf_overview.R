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
                  tags$span("2019–2023"),
                  tags$span(" | "),
                  
                  tags$strong("AIRLINES: "),
                  tags$span("All airlines")
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3("3.0M"),
                  tags$strong("Flights")
                  
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3("82.47%"),
                  tags$strong("On-time Rate")
              )
            ),
            column(
              width = 2,
              div(class = "eda-card eda-card-text",
                  tags$h3("17.53%"),
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
          div(class = "eda-card",
              style = "margin-bottom: 20px;",
              h3("Chart: MONTHLY DISTRIBUTION OF ON-TIME & DELAYED FLIGHTS"),
              p("Nội dung cột 1-2")
          ),
          div(class = "eda-card",
              style = "margin-bottom: 20px;",
              h3("Chart: MONTHLY FLIGHT VOLUME VS AVERAGE DELAY"),
              p("Nội dung cột 1-2")
          ),
          div(class = "eda-card",
              style = "margin-bottom: 20px;",
              h3("Chart: Delay Causes"),
              p("Nội dung cột 1-2")
          )
        )
      )
  )
)
