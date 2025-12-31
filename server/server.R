server <- function(input, output, session) {
  #==================================#
  # NAV_EDA_OVERVIEW.R
  #==================================#
  
  #----------------------------------#
  # OVERVIEW METRICS: CHANGE
  #----------------------------------#
  output$ov_flight_change <- renderUI({
    metric_trend(pct_23_19_flight, "Flights change:")
  })
  
  output$ov_cancel_change <- renderUI({
    metric_trend(pct_23_19_cancel, "Cancelled change:")
  })
  
  output$ov_divert_change <- renderUI({
    metric_trend(pct_23_19_divert, "Diverted change:")
  })
  
  output$ov_ontime_change <- renderUI({
    metric_trend(pct_23_19_ontime, "On-time change:")
  })
  
  output$ov_delay_change <- renderUI({
    metric_trend(pct_23_19_delayed, "Delayed change:")
  })
  
  #----------------------------------#
  # OVERVIEW METRIC: TOTAL
  #----------------------------------#
  session$onFlushed(function() {
    session$sendCustomMessage("animate", list(
      id = "ov_total_flights",
      value = total_flights
    ))
    
    session$sendCustomMessage("animate", list(
      id = "ov_total_airlines",
      value = total_airlines
    ))
    
    session$sendCustomMessage("animate", list(
      id = "ov_total_airports",
      value = total_airports
    ))
    
    session$sendCustomMessage("animate", list(
      id = "ov_total_routes",
      value = total_routes
    ))
  }, once = TRUE)
  
  # output$ov_tt_flights <- renderUI({
  #   tags$span(total_flights_fmt)
  # })
  # 
  # output$ov_tt_airlines <- renderUI({
  #   tags$span(total_airlines)
  # })
  # 
  # output$ov_tt_airports <- renderUI({
  #   tags$span(total_airports)
  # })
  # 
  # output$ov_tt_routes <- renderUI({
  #   tags$span(total_routes)
  # })
  
  #----------------------------------#
  # OVERVIEW CHARTS
  # Chart: Airline Rankings
  #----------------------------------#
  output$ov_airline_rank <- renderPlotly({
    fig_airline_rank
  })
  
  
  
  
  
  
  
}
