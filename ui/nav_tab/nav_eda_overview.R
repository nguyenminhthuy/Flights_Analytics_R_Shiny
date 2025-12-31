nav_eda_overview <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  div(class = "bslib-page-main page-bg",
      div(class = "container-fluid",
          fluidRow(
            column(
              width = 6,
              column(
                width = 6,
                div(class = "eda-card eda-card-text",
                    style = "margin-bottom: 20px;",
                    h4("2019-2023 Overview"),
                    p(uiOutput("flight_change")),
                    p(uiOutput("cancel_change")),
                    p(uiOutput("divert_change")),
                    p(uiOutput("ontime_change")),
                    p(uiOutput("delay_change"))
                ),
                div(
                  column(
                    width = 6,
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(uiOutput("ov_tt_flights")),
                        tags$strong("Total flights")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(uiOutput("ov_tt_airlines")),
                        tags$strong("Total airlines")
                    )
                  ),
                  column(
                    width = 6,
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(uiOutput("ov_tt_airports")),
                        tags$strong("Total airports")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(uiOutput("ov_tt_routes")),
                        tags$strong("Total routes")
                    )
                  )
                  
                )
              ),
              column(
                width = 6,
                div(class = "eda-card",
                    style = "margin-bottom: 20px;",
                    h4("Chart: Airline Ranking"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart"),
                    p("Chart")
                )
              )
            ),
            column(
              width = 6,
              div(class = "eda-card",
                  style = "margin-bottom: 20px;",
                  h4("Chart: Flight Distance Distribution"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
                  p("Chart"),
              ),
              div(class = "eda-card",
                  style = "margin-bottom: 20px;",
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
                   column(width = 5,
                          div(class = "eda-card",
                              h4("Chart: Weekly Distribution"),
                              p("Chart"),
                              p("Chart"),
                              p("Chart"),
                              p("Chart"),
                              p("Chart")
                          )
                   ),
                   column(width = 7,
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
