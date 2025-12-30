nav_template <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  div(class = "bslib-page-main page-bg",
      div(class = "container-fluid",
          fluidRow(
            column(
              width = 6,
              column(
                width = 6,
                div(class = "eda-card",
                    h4("Development 2019-2023"),
                    p("Nội dung cột 1-2")
                )
              ),
              column(
                width = 6,
                div(class = "eda-card",
                    h4("Airline ranking"),
                    p("Nội dung cột 1-2")
                )
              )
            ),
            column(
              width = 6,
              div(class = "eda-card",
                  h4("Flight Distance Distribution"),
                  p("Nội dung cột 1-2")
              )
            )
          )
      )
  )
)
