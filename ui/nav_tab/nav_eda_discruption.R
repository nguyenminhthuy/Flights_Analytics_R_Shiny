nav_eda_discruption <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  div(class = "bslib-page-main page-bg",
      div(class = "container-fluid",
          fluidRow(
            column(
              width = 6,
              column(
                width = 6,
                div(class = "eda-card",
                    h4("dis Cột 1-1"),
                    p("Nội dung cột 1-2")
                )
              ),
              column(
                width = 6,
                div(class = "eda-card",
                    h4("Cột 1-2"),
                    p("Nội dung cột 1-2")
                )
              )
            ),
            column(
              width = 6,
              div(class = "eda-card",
                  h4("Cột 2-2"),
                  p("Nội dung cột 1-2")
              )
            )
          )
      )
  )
)
