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
                    style = "margin-bottom: 40px;",
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
                        tags$h3(tags$span(id = "ov_total_flights",
                                          style = "font-size: 35px;")),
                        tags$strong("Total flights")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_airlines",
                                          style = "font-size: 35px;")),
                        tags$strong("Total airlines")
                    )
                  ),
                  column(
                    width = 6,
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_airports",
                                          style = "font-size: 35px;")),
                        tags$strong("Total airports")
                    ),
                    div(class = "eda-card eda-card-text",
                        style = "margin-bottom: 20px;",
                        tags$h3(tags$span(id = "ov_total_routes",
                                          style = "font-size: 35px;")),
                        tags$strong("Total routes")
                    )
                  )
                  
                )
              ),
              column(
                width = 6,
                # Chart: Airline Rankings
                div(class = "eda-card",
                    style = "margin-bottom: 20px;",
                    plotlyOutput("ov_airline_rank", height = 500)
                )
              )
            ),
            column(
              width = 6,
              # Chart: Flight Distance Distribution
              div(class = "eda-card",
                  style = "margin-bottom: 20px;",
                  plotlyOutput("ov_distance_dist", height = 200)
              ),
              # Chart: National Coverage
              div(class = "eda-card",
                  style = "margin-bottom: 20px;",
                  leafletOutput("ov_national_coverage", height = 260)
              )
            )
          ),
          fluidRow(style = "margin-bottom: 20px;",
                   column(width = 5,
                          # Chart: Weekly Distribution
                          div(class = "eda-card",
                              plotlyOutput("ov_dow_dist")
                          )
                   ),
                   column(width = 7,
                          # Chart: Monthly Departures
                          div(class = "eda-card",
                              plotlyOutput("ov_monthly_departure")
                          )
                   )
          )
      )
  )
)
