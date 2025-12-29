nav_eda_overview <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  div(class = "bslib-page-main page-bg",
      div(class = "container-fluid",
          fluidRow(style = "margin-bottom: 20px;",
            column(width = 6,
              column(width = 4,
                div(class = "eda-card eda-card-text",
                    h4("2019-2023 Overview"),
                    p("Flights nbrs:; +8.62"),
                    p("Cancelled: +8.62"),
                    p("Delays:; +8.62"),
                    p("Diverted: +8.62"),
                    p("On-time: +8.62")
                )
              ),
              column(width = 8,
                div(class = "eda-card",
                    h4("Chart: Airline Ranking"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart")
                )
              )
            ),
            column(width = 6,
              div(class = "eda-card",
                  h4("Chart: Flight Distance Distribution"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
              )
            )
          ),
          fluidRow(style = "margin-bottom: 20px;",
            column(width = 6,
                   column(width = 3,
                    div(class = "eda-card eda-card-text",
                      h2("6.74M"),
                      p("Flights")
                    )
                ),
                
                column(width = 3,
                    div(class = "eda-card eda-card-text",
                      h2("15"),
                      p("Airlines")
                    )
                ),
                
                column(width = 3,
                    div(class = "eda-card eda-card-text",
                      h2("13.48"),
                      p("Avg Departure Delay")
                    )
                ),
                
                column(width = 3,
                    div(class = "eda-card eda-card-text",
                      h2("350"),
                      p("Airports")
                    )
                ),
            ),
            column(width = 6,
                div(class = "eda-card",
                  h4("Chart: National Coverage"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart")
                )
            )
          ),
          fluidRow(style = "margin-bottom: 20px;",
              column(width = 3,
                div(class = "eda-card",
                    h4("Chart: Weekly Distribution"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart")
                  )
                ),
              column(width = 9,
                div(class = "eda-card",
                    h4("Chart: Monthly Departures"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart")
                  )
              )
          )
      )
  )
)
