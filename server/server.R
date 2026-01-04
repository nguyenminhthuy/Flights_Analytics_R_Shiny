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
  
  # ---- Applied filters
  pf_ov_applied_year    <- reactiveVal("All")
  pf_ov_applied_airline <- reactiveVal("All")
  
  observeEvent(input$pf_ov_apply_filter, {
    pf_ov_applied_year(input$pf_ov_year_select)
    pf_ov_applied_airline(input$pf_ov_airline_select)
  }, ignoreInit = TRUE)
  
  # ---- Central filtered dataset (CACHE)
  pf_ov_df <- reactive({
    req(df_flights)
    
    df <- df_flights
    
    if (pf_ov_applied_year() != "All") {
      df <- df[df$YEAR == pf_ov_applied_year(), ]
    }
    
    if (pf_ov_applied_airline() != "All") {
      df <- df[df$AIRLINE == pf_ov_applied_airline(), ]
    }
    
    df
  }) |> bindCache(
    pf_ov_applied_year(),
    pf_ov_applied_airline()
  )
  
  # ---- Summary (KPI numbers) – compute ONCE
  pf_ov_summary <- reactive({
    df <- pf_ov_df()
    
    cancelled <- df$CANCELLED == 1
    diverted  <- df$DIVERTED  == 1
    operated  <- !cancelled & !diverted
    on_time   <- operated & df$DEP_DELAY <= 15
    
    total_operated <- sum(operated)
    
    list(
      total_flights = nrow(df),
      on_time_rate  = if (total_operated > 0)
        round(sum(on_time) / total_operated * 100, 2) else NA,
      delay_rate    = if (total_operated > 0)
        round((total_operated - sum(on_time)) / total_operated * 100, 2) else NA
    )
  }) |> bindCache(
    pf_ov_applied_year(),
    pf_ov_applied_airline()
  )
  
  # ---- Charts (NO filtering inside)
  pf_ov_monthly_volume_delay <- reactive({
    monthly_volume_delay(pf_ov_df())
  }) |> bindCache(
    pf_ov_applied_year(),
    pf_ov_applied_airline()
  )
  
  pf_ov_delay_causes_chart <- reactive({
    delay_causes(pf_ov_df())
  }) |> bindCache(
    pf_ov_applied_year(),
    pf_ov_applied_airline()
  )
  
  # ==============================
  # OUTPUTS
  # ==============================
  # ---- KPI cards
  output$pf_ov_total_flights <- renderText({
    format_compact(pf_ov_summary()$total_flights)
  })
  
  output$pf_ov_on_time_rate <- renderText({
    paste0(pf_ov_summary()$on_time_rate, "%")
  })
  
  output$pf_ov_delay_rate <- renderText({
    paste0(pf_ov_summary()$delay_rate, "%")
  })
  
  # ---- Charts
  output$pf_ov_monthly_volume_delay <- renderPlotly({
    pf_ov_monthly_volume_delay()
  })
  
  output$pf_ov_delay_causes <- renderPlotly({
    pf_ov_delay_causes_chart()
  })
  
  # ---- Filter text
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
  
  # ---- Applied filters
  pf_lp_applied_year    <- reactiveVal("All")
  pf_lp_applied_airline <- reactiveVal("All")
  pf_lp_applied_origin  <- reactiveVal("All")
  pf_lp_applied_season  <- reactiveVal("All")
  
  observeEvent(input$pf_lp_apply_filter, {
    pf_lp_applied_year(input$pf_lp_year_select)
    pf_lp_applied_airline(input$pf_lp_airline_select)
    pf_lp_applied_origin(input$pf_lp_airport_select)
    pf_lp_applied_season(input$pf_lp_season_select)
  }, ignoreInit = TRUE)
  
  # ---- Central filtered dataset (CACHE)
  pf_lp_df <- reactive({
    req(df_flights)
    
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
  }) |> bindCache(
    pf_lp_applied_year(),
    pf_lp_applied_airline(),
    pf_lp_applied_origin(),
    pf_lp_applied_season()
  )
  
  # ---- Summary (KPI cards)
  pf_lp_summary <- reactive({
    local_patterns(pf_lp_df())
  }) |> bindCache(
    pf_lp_applied_year(),
    pf_lp_applied_airline(),
    pf_lp_applied_origin(),
    pf_lp_applied_season()
  )
  
  # ---- Airport stability table
  pf_lp_airport_stability_tbl <- reactive({
    airport_delay_stability(
      pf_lp_df(),
      min_flights = 1000
    )
  }) |> bindCache(
    pf_lp_applied_year(),
    pf_lp_applied_airline(),
    pf_lp_applied_origin(),
    pf_lp_applied_season()
  )
  
  # ---- Routing ranking table
  pf_lp_routing_tbl <- reactive({
    routing_ranking(pf_lp_df())
  }) |> bindCache(
    pf_lp_applied_year(),
    pf_lp_applied_airline(),
    pf_lp_applied_origin(),
    pf_lp_applied_season()
  )
  
  # ==============================
  # OUTPUTS
  # ==============================
  
  # ---- KPI cards
  output$pf_lp_total_flights <- renderText({
    format_compact(pf_lp_summary()$total_flights)
  })
  
  output$pf_lp_dep_delay <- renderText({
    paste0(pf_lp_summary()$avg_dep_delay, " min")
  })
  
  output$pf_lp_arr_delay <- renderText({
    paste0(pf_lp_summary()$avg_arr_delay, " min")
  })
  
  # ---- Airport stability table
  output$pf_lp_airport_stability_table <- DT::renderDT({
    DT::datatable(
      pf_lp_airport_stability_tbl(),
      rownames = FALSE,
      extensions = "Scroller",
      options = list(
        scrollY = 250,
        scrollCollapse = TRUE,
        scroller = TRUE,
        pageLength = 10,
        autoWidth = FALSE
      ),
      class = "stripe hover compact"
    )
  })
  
  # ---- Routing table
  output$pf_lp_routing_table <- DT::renderDT({
    DT::datatable(
      pf_lp_routing_tbl(),
      rownames = FALSE,
      extensions = "Scroller",
      options = list(
        scrollY = 250,
        scrollCollapse = TRUE,
        scroller = TRUE,
        pageLength = 10,
        autoWidth = FALSE
      ),
      class = "stripe hover compact"
    )
  })
  
  # ---- Charts
  output$pf_lp_time_of_day <- renderPlotly({
    time_of_day(pf_lp_df())
  })
  
  output$pf_lp_arrival_delay_map <- renderLeaflet({
    arrival_delay_folium(pf_lp_df())
  })
  
  # ---- Filter text
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
  
  # ---- Applied filters (snapshot khi bấm Apply)
  pf_ft_applied_year    <- reactiveVal("All")
  pf_ft_applied_airline <- reactiveVal("All")
  pf_ft_applied_season  <- reactiveVal("All")
  
  observeEvent(input$pf_ft_apply_filter, {
    pf_ft_applied_year(input$pf_ft_year_select)
    pf_ft_applied_airline(input$pf_ft_airline_select)
    pf_ft_applied_season(input$pf_ft_season_select)
  }, ignoreInit = TRUE)
  
  # ---- Central filtered dataset (CACHE)
  pf_ft_df <- reactive({
    req(df_flights)
    
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
  }) |> bindCache(
    pf_ft_applied_year(),
    pf_ft_applied_airline(),
    pf_ft_applied_season()
  )
  
  # ---- Charts (NO filtering inside)
  pf_ft_influence_of_delays_chart <- reactive({
    influence_of_delays(pf_ft_df())
  }) |> bindCache(
    pf_ft_applied_year(),
    pf_ft_applied_airline(),
    pf_ft_applied_season()
  )
  
  pf_ft_delay_factor_interaction_chart <- reactive({
    delay_factor_interaction(pf_ft_df())
  }) |> bindCache(
    pf_ft_applied_year(),
    pf_ft_applied_airline(),
    pf_ft_applied_season()
  )
  
  # ==============================
  # OUTPUTS
  # ==============================
  
  output$pf_ft_influence_of_delays <- renderPlotly({
    pf_ft_influence_of_delays_chart()
  })
  
  output$pf_ft_delay_factor_interaction <- renderPlotly({
    pf_ft_delay_factor_interaction_chart()
  })
  
  # ---- Filter text
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
  # DISRUPTION
  # ==============================
  
  # ---- Applied filters (snapshot khi bấm Apply)
  dis_applied_year    <- reactiveVal("All")
  dis_applied_airline <- reactiveVal("All")
  dis_applied_season  <- reactiveVal("All")
  
  observeEvent(input$dis_apply_filter, {
    dis_applied_year(input$dis_year_select)
    dis_applied_airline(input$dis_airline_select)
    dis_applied_season(input$dis_season_select)
  }, ignoreInit = TRUE)
  
  # ---- Central filtered dataset (CACHE)
  dis_df <- reactive({
    req(df_flights)
    
    df <- df_flights
    
    if (dis_applied_year() != "All") {
      df <- df[df$YEAR == dis_applied_year(), ]
    }
    
    if (dis_applied_airline() != "All") {
      df <- df[df$AIRLINE == dis_applied_airline(), ]
    }
    
    if (dis_applied_season() != "All") {
      df <- df[df$SEASON == dis_applied_season(), ]
    }
    
    df
  }) |> bindCache(
    dis_applied_year(),
    dis_applied_airline(),
    dis_applied_season()
  )
  
  # ---- Summary KPIs
  dis_summary <- reactive({
    disruption_metrics(dis_df())
  }) |> bindCache(
    dis_applied_year(),
    dis_applied_airline(),
    dis_applied_season()
  )
  
  # ---- Charts
  dis_disruption_bar_chart <- reactive({
    plot_disruption_bar(dis_df())
  }) |> bindCache(
    dis_applied_year(),
    dis_applied_airline(),
    dis_applied_season()
  )
  
  dis_cause_donut_chart <- reactive({
    plot_cause_donut(dis_df())
  }) |> bindCache(
    dis_applied_year(),
    dis_applied_airline(),
    dis_applied_season()
  )
  
  # ==============================
  # OUTPUTS
  # ==============================
  
  # ---- KPIs
  output$dis_total_flight <- renderText({
    format_compact(dis_summary()$dis_total_flight)
  })
  
  output$dis_cancel_flight <- renderText({
    format_compact(dis_summary()$dis_cancel_flight)
  })
  
  output$dis_divert_flight <- renderText({
    format_compact(dis_summary()$dis_divert_flight)
  })
  
  # ---- Charts
  output$dis_plot_disruption_bar <- renderPlotly({
    dis_disruption_bar_chart()
  })
  
  output$dis_plot_cause_donut <- renderPlotly({
    dis_cause_donut_chart()
  })
  
  # ---- Filter text
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
