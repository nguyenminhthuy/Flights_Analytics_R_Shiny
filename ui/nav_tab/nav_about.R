nav_about <- tagList(
  
  # LOAD CSS
  header = tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  div(class = "about-page",
    sidebarLayout(
      # -------- Sidebar --------
      sidebarPanel(
        width = 3,
        class = "sidebar",
        
        h4(icon("circle-info"), " About Dataset"),
        p("Overview of the dataset used in this dashboard."),
        
        hr(),
        tags$small(
          icon("database"),
          " Source: Kaggle / US DOT"
        ),
        hr(),
        tags$small(
          "Prepared by: ",
          tags$strong("Thuy Nguyen")
        ),
        br(),
        tags$small(
          "Last updated: ",
          tags$strong("Jan 2026")
        ),
        br(), br()
      ),
      
      # -------- Main Panel --------
      mainPanel(
        width = 9,
        
        tags$h3("Flight Delay and Cancellation Dataset (2019–2023)"),
        
        tags$p(
          tags$strong("Sample size: "),
          textOutput("sample_size", inline = TRUE),
          " (~29 million rows, sampled ~3M).",
          br(),
          "Data period: January 2019 – August 2023."
        ),
        
        tags$hr(),
        
        tags$h4("📊 Dataset Overview"),
        tags$p(
          "This dataset contains detailed information on U.S. domestic flights, including flight schedules, delays, cancellations, and operational attributes."
        ),
        
        tags$p(
          "The data was sourced from ",
          tags$a(
            href = "https://www.kaggle.com/datasets/patrickzel/flight-delay-and-cancellation-dataset-2019-2023/data",
            "Kaggle",
            target = "_blank"
          ),
          ", originally published by the U.S. Department of Transportation."
        ),
        
        tags$hr(),
        
        tags$h4("🧩 Key Variables"),
        tags$table(
          class = "table table-striped table-hover dataset-table",
          tags$thead(
            tags$tr(
              tags$th("Variable"),
              tags$th("Description"),
              tags$th("Notes")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$code("FL_DATE")),
              tags$td("Flight date"),
              tags$td("Format: YYYY-MM-DD")
            ),
            tags$tr(
              tags$td(tags$code("AIRLINE")),
              tags$td("Airline name"),
              tags$td("Carrier operating the flight")
            ),
            tags$tr(
              tags$td(tags$code("ORIGIN / DEST")),
              tags$td("Origin and destination airports"),
              tags$td("IATA airport codes")
            ),
            tags$tr(
              tags$td(tags$code("CRS_DEP_TIME / DEP_TIME")),
              tags$td("Scheduled and actual departure time"),
              tags$td("Local time, HHMM format")
            ),
            tags$tr(
              tags$td(tags$code("DEP_DELAY / ARR_DELAY")),
              tags$td("Delay duration"),
              tags$td("Minutes; negative = early")
            ),
            tags$tr(
              tags$td(tags$code("CANCELLED")),
              tags$td("Cancellation indicator"),
              tags$td("1 = cancelled, 0 = not cancelled")
            ),
            tags$tr(
              tags$td(tags$code("DIVERTED")),
              tags$td("Diversion indicator"),
              tags$td("1 = diverted, 0 = not diverted")
            ),
            tags$tr(
              tags$td(tags$code("AIR_TIME")),
              tags$td("Time in air"),
              tags$td("Minutes")
            ),
            tags$tr(
              tags$td(tags$code("DISTANCE")),
              tags$td("Flight distance"),
              tags$td("Miles")
            )
          )
        ),
        
        tags$hr(),
        tags$h4("📌 Data Source"),
        tags$p(
          "Original data provided by the ",
          tags$a(
            href = "https://www.transtats.bts.gov",
            "U.S. Department of Transportation – Bureau of Transportation Statistics",
            target = "_blank"
          )
        ),
        
        tags$hr(),
        h4(icon("sticky-note"), "Notes"),
        tags$ul(
          tags$li("Data for 2023 is available only through August."),
          tags$li("This dashboard is intended for exploratory analysis, not for evaluating airline performance or service quality.")
        )
      )
    )
  )
)
