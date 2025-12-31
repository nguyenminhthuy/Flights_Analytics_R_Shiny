nav_eda_overview <- tagList(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  
  tags$script(HTML("
  /*==================================
    Format number to K / M
    ==================================*/
  function formatNumber(num) {
    if (num >= 1e6) {
      return (num / 1e6).toFixed(1).replace('.0', '') + 'M';
    }
    if (num >= 1e3) {
      return (num / 1e3).toFixed(1).replace('.0', '') + 'K';
    }
    return num.toString();
  }
 
  /*==================================
    ANIMATE NUMBER COUNTING UP EFFECT
    ==================================*/
    
    function animateValue(id, start, end, duration) {
      const obj = document.getElementById(id);
      if (!obj) return;
    
      let startTimestamp = null;
    
      const step = (timestamp) => {
        if (!startTimestamp) startTimestamp = timestamp;
    
        const progress = Math.min((timestamp - startTimestamp) / duration, 1);
        const value = Math.floor(progress * (end - start) + start);
    
        obj.innerHTML = formatNumber(value);
    
        if (progress < 1) {
          window.requestAnimationFrame(step);
        }
      };
    
      window.requestAnimationFrame(step);
    }
    
    /*==================================
      Receive data from Shiny and trigger animation
      ==================================*/
    Shiny.addCustomMessageHandler('animate', function(message) {
      animateValue(message.id, 0, message.value, 2500); // 👈 animation duration (ms)
    });
    
    ")),
  
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
                    p(uiOutput("ov_flight_change")),
                    p(uiOutput("ov_cancel_change")),
                    p(uiOutput("ov_divert_change")),
                    p(uiOutput("ov_ontime_change")),
                    p(uiOutput("ov_delay_change"))
                ),
                div(
                  column(
                    width = 6,
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_flights")),
                        tags$strong("Total flights")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_airlines")),
                        tags$strong("Total airlines")
                    )
                  ),
                  column(
                    width = 6,
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_airports")),
                        tags$strong("Total airports")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_routes")),
                        tags$strong("Total routes")
                    )
                  )
                  
                )
              ),
              column(
                width = 6,
                div(class = "eda-card",
                    style = "margin-bottom: 20px;",
                    #h4("Chart: Airline Ranking"),
                    #p("Chart"),
                    plotlyOutput("ov_airline_rank")
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
