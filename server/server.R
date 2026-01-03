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
  
  # ==============================
  # PERFORMANCE - OVERVIEW
  # ==============================
  
  pf_ov_applied_year    <- reactiveVal("All")
  pf_ov_applied_airline <- reactiveVal("All")
  
  observeEvent(input$pf_ov_apply_filter, {
    pf_ov_applied_year(input$pf_ov_year_select)
    pf_ov_applied_airline(input$pf_ov_airline_select)
  })
  
  #----------------------------------#
  # RUN ONLY ON APPLY
  #----------------------------------#
  pf_ov_filtered_flights <- reactive({
    
    # Page load → no filter
    if (input$pf_ov_apply_filter == 0) {
      return(df_flights)
    }
    
    df <- df_flights
    
    if (pf_ov_applied_year() != "All") {
      df <- df[df$YEAR == pf_ov_applied_year(), ]
    }
    
    if (pf_ov_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_ov_applied_airline(), ]
    }
    
    df
  })
  
  #----------------------------------#
  # SUMMARY (FAST, NO FILTER HERE)
  #----------------------------------#
  pf_ov_summary_data <- reactive({
    
    df <- pf_ov_filtered_flights()
    
    total_flights <- nrow(df)
    
    cancelled <- df$CANCELLED == 1
    diverted  <- df$DIVERTED  == 1
    operated  <- !cancelled & !diverted
    on_time   <- operated & df$DEP_DELAY <= 15
    
    total_operated <- sum(operated)
    total_on_time  <- sum(on_time)
    
    list(
      total_flights = total_flights,
      on_time_rate  = round(total_on_time / total_operated * 100, 2),
      delay_rate    = round((total_operated - total_on_time) / total_operated * 100, 2)
    )
  })
  
  #----------------------------------#
  # RENDER CARDS
  #----------------------------------#
  output$pf_ov_total_flights <- renderText({
    format_compact(pf_ov_summary_data()$total_flights)
  })
  
  output$pf_ov_on_time_rate <- renderText({
    paste0(pf_ov_summary_data()$on_time_rate, "%")
  })
  
  output$pf_ov_delay_rate <- renderText({
    paste0(pf_ov_summary_data()$delay_rate, "%")
  })
  
  #----------------------------------#
  # RENDER TEXT 
  #----------------------------------#
  output$pf_ov_year_text <- renderText({
    if (pf_ov_applied_year() == "All") "2019–2023" 
    else pf_ov_applied_year()
  })
  
  output$pf_ov_airline_text <- renderText({
    if (pf_ov_applied_airline() == "All") "All airlines" 
    else pf_ov_applied_airline()
  })
  
  # ==============================
  # PERFORMANCE - LOCAL PATTERNS
  # ==============================
  
  pf_lp_applied_year    <- reactiveVal("All")
  pf_lp_applied_airline <- reactiveVal("All")
  pf_lp_applied_origin  <- reactiveVal("All")
  pf_lp_applied_season  <- reactiveVal("All")
  
  observeEvent(input$pf_lp_apply_filter, {
    pf_lp_applied_year(input$pf_lp_year_select)
    pf_lp_applied_airline(input$pf_lp_airline_select)
    pf_lp_applied_origin(input$pf_lp_airport_select)
    pf_lp_applied_season(input$pf_lp_season_select)
  })
  
  pf_lp_summary_data <- reactive({
    
    # Page load → no filter
    if (input$pf_lp_apply_filter == 0) {
      return(local_patterns(df_flights))
    }
    
    local_patterns(
      df_flights,
      year = if (pf_lp_applied_year() == "All") NULL else pf_lp_applied_year(),
      airline = if (pf_lp_applied_airline() == "All") NULL else pf_lp_applied_airline(),
      origin_airport = if (pf_lp_applied_origin() == "All") NULL else pf_lp_applied_origin(),
      season = if (pf_lp_applied_season() == "All") NULL else pf_lp_applied_season()
    )
  })
  
  output$pf_lp_total_flights <- renderText({
    format_compact(pf_lp_summary_data()$total_flights)
  })
  
  output$pf_lp_dep_delay <- renderText({
    paste0(pf_lp_summary_data()$avg_dep_delay, " min")
  })
  
  output$pf_lp_arr_delay <- renderText({
    paste0(pf_lp_summary_data()$avg_arr_delay, " min")
  })
  
  output$pf_lp_year_text <- renderText({
    if (pf_lp_applied_year() == "All") "2019–2023"
    else pf_lp_applied_year()
  })
  
  output$pf_lp_airline_text <- renderText({
    if (pf_lp_applied_airline() == "All") "All airlines"
    else pf_lp_applied_airline()
  })
  
  output$pf_lp_origin_text <- renderText({
    if (pf_lp_applied_origin() == "All") "All airports"
    else pf_lp_applied_origin()
  })
  
  output$pf_lp_season_text <- renderText({
    if (pf_lp_applied_season() == "All") "All seasons"
    else pf_lp_applied_season()
  })
  
  # ==============================
  # PERFORMANCE - FACTORS
  # ==============================
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
  
  # ==============================
  # DISCRUPTION
  # ==============================
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
