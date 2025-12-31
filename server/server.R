server <- function(input, output, session) {
  #==================================#
  # NAV_EDA_OVERVIEW.R
  #==================================#
  
  #----------------------------------#
  # OVERVIEW METRICS
  #----------------------------------#
  output$flight_change <- renderUI({
    metric_trend(pct_23_19_flight, "Flights change:")
  })
  
  output$cancel_change <- renderUI({
    metric_trend(pct_23_19_cancel, "Cancelled change:")
  })
  
  output$divert_change <- renderUI({
    metric_trend(pct_23_19_divert, "Diverted change:")
  })
  
  output$ontime_change <- renderUI({
    metric_trend(pct_23_19_ontime, "On-time change:")
  })
  
  output$delay_change <- renderUI({
    metric_trend(pct_23_19_delayed, "Delayed change:")
  })
  
  output$ov_tt_flights <- renderUI({
    tags$span(total_flights_fmt)
  })
  
  output$ov_tt_airlines <- renderUI({
    tags$span(total_airlines)
  })
  
  output$ov_tt_airports <- renderUI({
    tags$span(total_airports)
  })
  
  output$ov_tt_routes <- renderUI({
    tags$span(total_routes)
  })
  
  #----------------------------------#
  # OVERVIEW CHARTS
  #----------------------------------#
  
  
  
  
  
  
  
  
}
