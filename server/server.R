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
  
  #----------------------------------#
  # OVERVIEW CHARTS
  #----------------------------------#
  # Chart: Airline Rankings
  output$ov_airline_rank <- renderPlotly({
    fig_airline_rank
  })
  
  # Chart: Flight Distance Distribution
  output$ov_distance_dist <- renderPlotly({
    fig_distance_dist
  })
  
  # Chart: National Coverage
  output$ov_national_coverage <- renderLeaflet({
    fig_national_coverage
  })
  
  # Chart: Weekly Distribution
  output$ov_dow_dist <- renderPlotly({
    fig_dow_dist
  })
  
  # Chart: Monthly Departures
  output$ov_monthly_departure <- renderPlotly({
    fig_monthly_departures
  })
  
  #==================================#
  # LOAD DATA TO SELECT INPUT (ALL PAGES)
  #==================================#
  observe({
    # YEAR
    years <- sort(unique(df_flights$YEAR))
    
    updateSelectInput(
      session,
      "pf_ov_year_select",
      choices = c("All", years),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "pf_lp_year_select",
      choices = c("All", years),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "pf_ft_year_select",
      choices = c("All", years),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "dis_year_select",
      choices = c("All", years),
      selected = "All"
    )
    
    # AIRLINE
    airlines <- sort(unique(df_flights$AIRLINE))
    
    updateSelectInput(
      session,
      "pf_ov_airline_select",
      choices = c("All", airlines),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "pf_lp_airline_select",
      choices = c("All", airlines),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "pf_ft_airline_select",
      choices = c("All", airlines),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "dis_airline_select",
      choices = c("All", airlines),
      selected = "All"
    )
    
    # ORIGIN CITY
    origin <- sort(unique(df_flights$ORIGIN_CITY))
    
    updateSelectInput(
      session,
      "pf_lp_airport_select",
      choices = c("All", origin),
      selected = "All"
    )
    
    # SEASON
    season <- sort(unique(df_flights$SEASON))
    
    updateSelectInput(
      session,
      "pf_lp_season_select",
      choices = c("All", season),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "pf_ft_season_select",
      choices = c("All", season),
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "dis_season_select",
      choices = c("All", season),
      selected = "All"
    )
  })
  
  # ===============================
  # APPLY FILTER
  # ===============================
  
  # Initial state
  display_year <- reactiveVal("2019–2023")
  display_airline <- reactiveVal("All airlines")
  display_origin <- reactiveVal("All airports")
  display_season <- reactiveVal("All seasons")
  
  #----------------------------------#
  # PERFORMANCE - OVERVIEW
  #----------------------------------#
  # Update on Apply
  observeEvent(input$pf_ov_apply_filter, {
    
    display_year(
      if (input$pf_ov_year_select == "All") {
        "2019–2023"
      } else {
        input$pf_ov_year_select
      }
    )
    
    display_airline(
      if (input$pf_ov_airline_select == "All") {
        "All airlines"
      } else {
        input$pf_ov_airline_select
      }
    )
  })
  
  # render output
  output$pf_ov_year_text <- renderText({
    display_year()
  })
  
  output$pf_ov_airline_text <- renderText({
    display_airline()
  })
  
  #----------------------------------#
  # PERFORMANCE - LOCAL PATTERNS
  #----------------------------------#
  # Update on Apply
  observeEvent(input$pf_lp_apply_filter, {
    
    display_year(
      if (input$pf_lp_year_select == "All") {
        "2019–2023"
      } else {
        input$pf_lp_year_select
      }
    )
    
    display_airline(
      if (input$pf_lp_airline_select == "All") {
        "All airlines"
      } else {
        input$pf_lp_airline_select
      }
    )
    
    display_origin(
      if (input$pf_lp_airport_select == "All") {
        "All airports"
      } else {
        input$pf_lp_airport_select
      }
    )
    
    display_season(
      if (input$pf_lp_season_select == "All") {
        "All seasons"
      } else {
        input$pf_lp_season_select
      }
    )
  })
  
  # render output
  output$pf_lp_year_text <- renderText({
    display_year()
  })
  
  output$pf_lp_airline_text <- renderText({
    display_airline()
  })
  
  output$pf_lp_origin_text <- renderText({
    display_origin()
  })
  
  output$pf_lp_season_text <- renderText({
    display_season()
  })
  
  #----------------------------------#
  # PERFORMANCE - FACTORS
  #----------------------------------#
  # Update on Apply
  observeEvent(input$pf_ft_apply_filter, {
    
    display_year(
      if (input$pf_ft_year_select == "All") {
        "2019–2023"
      } else {
        input$pf_ft_year_select
      }
    )
    
    display_airline(
      if (input$pf_ft_airline_select == "All") {
        "All airlines"
      } else {
        input$pf_ft_airline_select
      }
    )
    
    display_season(
      if (input$pf_ft_season_select == "All") {
        "All seasons"
      } else {
        input$pf_ft_season_select
      }
    )
  })
  
  # render output
  output$pf_ft_year_text <- renderText({
    display_year()
  })
  
  output$pf_ft_airline_text <- renderText({
    display_airline()
  })
  
  output$pf_ft_season_text <- renderText({
    display_season()
  })
  
  #----------------------------------#
  # DISCRUPTION
  #----------------------------------#
  # Update on Apply
  observeEvent(input$dis_apply_filter, {
    
    display_year(
      if (input$dis_year_select == "All") {
        "2019–2023"
      } else {
        input$dis_year_select
      }
    )
    
    display_airline(
      if (input$dis_airline_select == "All") {
        "All airlines"
      } else {
        input$dis_airline_select
      }
    )
    
    display_season(
      if (input$dis_season_select == "All") {
        "All seasons"
      } else {
        input$dis_season_select
      }
    )
  })
  
  # render output
  output$dis_year_text <- renderText({
    display_year()
  })
  
  output$dis_airline_text <- renderText({
    display_airline()
  })
  
  output$dis_season_text <- renderText({
    display_season()
  })
  
  
  
  
  
  
  
  
  
}
