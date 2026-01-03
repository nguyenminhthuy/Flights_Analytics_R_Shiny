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
  
  pf_ov_monthly_volume_delay <- reactive({
    
    # Page load → no filter
    if (input$pf_ov_apply_filter == 0) {
      return(
        monthly_volume_delay(df_flights)
      )
    }
    
    monthly_volume_delay(
      df_flights,
      year    = if (pf_ov_applied_year() == "All") NULL else pf_ov_applied_year(),
      airline = if (pf_ov_applied_airline() == "All") NULL else pf_ov_applied_airline()
    )
  })
  
  pf_ov_delay_causes_chart <- reactive({
    
    # Page load → no filter
    if (input$pf_ov_apply_filter == 0) {
      return(
        delay_causes(df_flights)
      )
    }
    
    delay_causes(
      df_flights,
      year    = if (pf_ov_applied_year() == "All") NULL else pf_ov_applied_year(),
      airline = if (pf_ov_applied_airline() == "All") NULL else pf_ov_applied_airline()
    )
  })
  
  # RENDER CARDS
  output$pf_ov_total_flights <- renderText({
    format_compact(pf_ov_summary_data()$total_flights)
  })
  
  output$pf_ov_on_time_rate <- renderText({
    paste0(pf_ov_summary_data()$on_time_rate, "%")
  })
  
  output$pf_ov_delay_rate <- renderText({
    paste0(pf_ov_summary_data()$delay_rate, "%")
  })
  
  # Render charts
  
  output$pf_ov_monthly_volume_delay <- renderPlotly({
    pf_ov_monthly_volume_delay()
  })
  
  output$pf_ov_delay_causes <- renderPlotly({
    pf_ov_delay_causes_chart()
  })
  
  # RENDER TEXT 
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
  
  pf_lp_filtered_flights <- reactive({
    
    # Page load → no filter
    if (input$pf_lp_apply_filter == 0) {
      return(df_flights)
    }
    
    df <- df_flights
    
    if (pf_lp_applied_year() != "All") {
      df <- df[df$YEAR == pf_lp_applied_year(), ]
    }
    
    if (pf_lp_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_lp_applied_airline(), ]
    }
    
    if (pf_lp_applied_origin() != "All") {
      df <- df[df$ORIGIN == pf_lp_applied_origin(), ]
    }
    
    if (pf_lp_applied_season() != "All") {
      df <- df[df$SEASON == pf_lp_applied_season(), ]
    }
    
    df
  })
  
  pf_lp_airport_stability <- reactive({
    
    airport_delay_stability(
      pf_lp_filtered_flights()
    )
    
  })
  
  pf_lp_routing_table <- reactive({
    
    df <- pf_lp_filtered_flights()
    
    routing_ranking(
      df,
      year    = if (pf_lp_applied_year() == "All") NULL else pf_lp_applied_year(),
      airline = if (pf_lp_applied_airline() == "All") NULL else pf_lp_applied_airline(),
      season  = if (pf_lp_applied_season() == "All") NULL else pf_lp_applied_season()
    )
  })
  
  pf_lp_filtered_flights <- reactive({
    
    if (input$pf_lp_apply_filter == 0) {
      return(df_flights)
    }
    
    df <- df_flights
    
    if (pf_lp_applied_year() != "All") {
      df <- df[df$YEAR == pf_lp_applied_year(), ]
    }
    
    if (pf_lp_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_lp_applied_airline(), ]
    }
    
    if (pf_lp_applied_origin() != "All") {
      df <- df[df$ORIGIN == pf_lp_applied_origin(), ]
    }
    
    if (pf_lp_applied_season() != "All") {
      df <- df[df$SEASON == pf_lp_applied_season(), ]
    }
    
    df
  })
  
  pf_lp_filtered_flights <- reactive({
    
    if (input$pf_lp_apply_filter == 0) {
      return(df_flights)
    }
    
    df <- df_flights
    
    if (pf_lp_applied_year() != "All") {
      df <- df[df$YEAR == pf_lp_applied_year(), ]
    }
    
    if (pf_lp_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_lp_applied_airline(), ]
    }
    
    if (pf_lp_applied_origin() != "All") {
      df <- df[df$ORIGIN == pf_lp_applied_origin(), ]
    }
    
    if (pf_lp_applied_season() != "All") {
      df <- df[df$SEASON == pf_lp_applied_season(), ]
    }
    
    df
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
  
  output$pf_lp_airport_stability_table <- DT::renderDT({
    
    df_tbl <- airport_delay_stability(
      df_flights,
      min_flights = 1000,
      year    = if (pf_lp_applied_year()    == "All") NULL else pf_lp_applied_year(),
      airline = if (pf_lp_applied_airline() == "All") NULL else pf_lp_applied_airline(),
      season  = if (pf_lp_applied_season()  == "All") NULL else pf_lp_applied_season(),
      origin  = if (pf_lp_applied_origin()  == "All") NULL else pf_lp_applied_origin()
    )
    
    DT::datatable(
      df_tbl,
      rownames = FALSE,        # ❌ bỏ cột index
      extensions = "Scroller",
      options = list(
        autoWidth = FALSE,
        scrollY = 250,
        scrollCollapse = TRUE,
        scroller = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(width = "70px",  targets = 0), # Airport
          list(width = "100px", targets = 1), # n_flights
          list(width = "100px", targets = 2), # avg delay
          list(width = "100px", targets = 3)  # std
        )
      ),
      class = "stripe hover compact"
    )
  })
  
  output$pf_lp_routing_table <- DT::renderDT({
    
    DT::datatable(
      pf_lp_routing_table(),
      rownames = FALSE,
      extensions = "Scroller",
      options = list(
        scrollY = 250,
        scrollCollapse = TRUE,
        scroller = TRUE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        autoWidth = FALSE,
        dom = "lftip",
        columnDefs = list(
          list(width = "140px", targets = 0),
          list(className = "dt-right", targets = c(1, 2))
        )
      ),
      class = "stripe hover compact"
    )
  })
  
  output$pf_lp_time_of_day <- renderPlotly({
    
    time_of_day(
      df = pf_lp_filtered_flights(),
      year    = if (pf_lp_applied_year() == "All") NULL else pf_lp_applied_year(),
      airline = if (pf_lp_applied_airline() == "All") NULL else pf_lp_applied_airline(),
      season  = if (pf_lp_applied_season() == "All") NULL else pf_lp_applied_season()
    )
  })
  
  output$pf_lp_arrival_delay_map <- renderLeaflet({
    
    arrival_delay_folium(
      df = pf_lp_filtered_flights(),
      year    = if (pf_lp_applied_year() == "All") NULL else pf_lp_applied_year(),
      airline = if (pf_lp_applied_airline() == "All") NULL else pf_lp_applied_airline(),
      season  = if (pf_lp_applied_season() == "All") NULL else pf_lp_applied_season()
    )
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
  pf_ft_applied_year    <- reactiveVal("All")
  pf_ft_applied_airline <- reactiveVal("All")
  pf_ft_applied_season  <- reactiveVal("All")
  
  observeEvent(input$pf_ft_apply_filter, {
    
    pf_ft_applied_year(input$pf_ft_year_select)
    pf_ft_applied_airline(input$pf_ft_airline_select)
    pf_ft_applied_season(input$pf_ft_season_select)
    
  })
  
  pf_ft_filtered_flights <- reactive({
    
    # Page load → no filter
    if (input$pf_ft_apply_filter == 0) {
      return(df_flights)
    }
    
    df <- df_flights
    
    if (pf_ft_applied_year() != "All") {
      df <- df[df$YEAR == pf_ft_applied_year(), ]
    }
    
    if (pf_ft_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_ft_applied_airline(), ]
    }
    
    if (pf_ft_applied_season() != "All") {
      df <- df[df$SEASON == pf_ft_applied_season(), ]
    }
    
    df
  })
  
  output$pf_ft_influence_of_delays <- renderPlotly({
    
    influence_of_delays(
      df = pf_ft_filtered_flights(),
      year    = if (pf_ft_applied_year() == "All") NULL else pf_ft_applied_year(),
      airline = if (pf_ft_applied_airline() == "All") NULL else pf_ft_applied_airline(),
      season  = if (pf_ft_applied_season() == "All") NULL else pf_ft_applied_season()
    )
  })
  
  output$pf_ft_delay_factor_interaction <- renderPlotly({
    
    delay_factor_interaction(
      df = pf_ft_filtered_flights(),
      year    = if (pf_ft_applied_year() == "All") NULL else pf_ft_applied_year(),
      airline = if (pf_ft_applied_airline() == "All") NULL else pf_ft_applied_airline(),
      season  = if (pf_ft_applied_season() == "All") NULL else pf_ft_applied_season()
    )
  })
  
  output$pf_ft_year_text <- renderText({
    if (pf_ft_applied_year() == "All") "2019–2023"
    else pf_ft_applied_year()
  })
  
  output$pf_ft_airline_text <- renderText({
    if (pf_ft_applied_airline() == "All") "All airlines"
    else pf_ft_applied_airline()
  })
  
  output$pf_ft_season_text <- renderText({
    if (pf_ft_applied_season() == "All") "All seasons"
    else pf_ft_applied_season()
  })
  
  
  
  # ==============================
  # DISCRUPTION
  # ==============================
  dis_applied_year    <- reactiveVal("All")
  dis_applied_airline <- reactiveVal("All")
  dis_applied_season  <- reactiveVal("All")
  
  observeEvent(input$dis_apply_filter, {
    dis_applied_year(input$dis_year_select)
    dis_applied_airline(input$dis_airline_select)
    dis_applied_season(input$dis_season_select)
  })
  
  dis_summary_data <- reactive({
    
    # Page load → no filter
    if (input$dis_apply_filter == 0) {
      return(disruption_metrics(df_flights))
    }
    
    disruption_metrics(
      df_flights,
      year    = if (dis_applied_year() == "All") NULL else dis_applied_year(),
      airline = if (dis_applied_airline() == "All") NULL else dis_applied_airline(),
      season  = if (dis_applied_season() == "All") NULL else dis_applied_season()
    )
  })
  
  output$dis_total_flight <- renderText({
    format_compact(dis_summary_data()$dis_total_flight)
  })
  
  output$dis_cancel_flight <- renderText({
    format_compact(dis_summary_data()$dis_cancel_flight)
  })
  
  output$dis_divert_flight <- renderText({
    format_compact(dis_summary_data()$dis_divert_flight)
  })
  
  output$dis_year_text <- renderText({
    if (dis_applied_year() == "All") "2019–2023"
    else dis_applied_year()
  })
  
  output$dis_airline_text <- renderText({
    if (dis_applied_airline() == "All") "All airlines"
    else dis_applied_airline()
  })
  
  output$dis_season_text <- renderText({
    if (dis_applied_season() == "All") "All seasons"
    else dis_applied_season()
  })
  
  
  
  
  
  
  
  
}
