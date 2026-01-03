library(arrow)
library(fs)
library(tidyverse)
library(dplyr)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(rlang)

options(dplyr.summarise.inform = FALSE)

#==================================#
# 1. LOAD DATA
# Load CSV/Parquet files into Polars DataFrames
# If Parquet exists, read it directly; otherwise read CSV, write Parquet, then read
#==================================#
csv_to_parquet <- function(csv_dir, parquet_dir, tables) {
  csv_dir <- path(csv_dir)
  parquet_dir <- path(parquet_dir)
  dir_create(parquet_dir)
  
  dfs <- list()
  
  for (table in tables) {
    parquet_path <- path(parquet_dir, paste0(table, ".parquet"))
    
    if (!file_exists(parquet_path)) {
      message("Creating parquet for: ", table)
      
      open_dataset(
        path(csv_dir, paste0(table, ".csv")),
        format = "csv"
      ) |>
        write_dataset(
          parquet_dir,
          format = "parquet",
          basename_template = paste0(table, ".parquet")
        )
    } 
    
    # read parquet
    dfs[[table]] <- read_parquet(parquet_path)
    message(
      table, ": ",
      nrow(dfs[[table]]), " rows, ",
      ncol(dfs[[table]]), " cols"
    )
  }
  dfs
}

tables <- c(
  "flights_sample_3m",
  "airports_clean"
)

dfs <- csv_to_parquet(
  csv_dir = "data/csv",
  parquet_dir = "data/parquet",
  tables = tables
)

df_flights <- dfs$flights_sample_3m
df_airports <- dfs$airports_clean

#==================================#
# 2. FEATURE ENGINEERING & DATA CLEANING
# 2.1 Feature Engineering
# Time & Distance Features
#==================================#
dow_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                "Friday", "Saturday", "Sunday")

df_flights <- df_flights |>
  mutate(
    FL_DATE = ymd(FL_DATE),
    YEAR = year(FL_DATE),
    MONTH = month(FL_DATE),
    QUARTER = quarter(FL_DATE),
    DAY_OF_WEEK = wday(FL_DATE), 
    
    DISTANCE_CAT = cut(
      DISTANCE,
      breaks = c(-Inf, 500, 1500, Inf),
      labels = c("Short-haul", "Medium-haul", "Long-haul")
    ),
    
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Autumn"
    ),
    ROUTE = paste0(ORIGIN_CITY, " → ", DEST_CITY),
    DEP_HOUR = as.integer(CRS_DEP_TIME %/% 100)
  )

#----------------------------------#
# Adding Geolocation Columns
#----------------------------------#
airports_ll <- df_airports |>
  filter(!is.na(IATA), IATA != "") |>
  select(IATA, LAT, LON)

df_flights <- df_flights |>
  left_join(airports_ll, join_by(ORIGIN == IATA)) |>
  rename(
    ORIGIN_LAT = LAT,
    ORIGIN_LON = LON
  ) |>
  left_join(airports_ll, join_by(DEST == IATA)) |>
  rename(
    DEST_LAT = LAT,
    DEST_LON = LON
  )

#==================================#
# 2.2 Check for missing values
#==================================#
check_missing_cols <- function(df){
  missing_cols <- c()
  missing_counts <- c()
  no_missing_cols <- c()
  
  for(col in names(df)){
    na_count <- sum(is.na(df[[col]]))
    if(na_count > 0){
      missing_cols <- c(missing_cols, col)
      missing_counts <- c(missing_counts, na_count)
    }
    else{
      no_missing_cols <- c(no_missing_cols, col)
    }
  }
  
  cat("Columns WITH missing values:\n")
  if (length(missing_cols) > 0) {
    for (i in seq_along(missing_cols)) {
      cat("-", missing_cols[i], ":", missing_counts[i], "\n")
    }
  } else {
    cat("None\n")
  }
  
  cat("\nColumns WITHOUT missing values:\n")
  if (length(no_missing_cols) > 0) {
    print(no_missing_cols)
  } else {
    cat("None\n")
  }
}

check_missing_cols(df_flights)

#----------------------------------#
# Remove na
#----------------------------------#
# df_flights <- df_flights |>
#   drop_na(ORIGIN_LAT, ORIGIN_LON,
#           DEST_LAT, DEST_LON)

#==================================#
# 2.3 Format numbers
#==================================#
format_compact <- function(n) {
  case_when(
    n >= 1e6 ~ sub("\\.0M$", "M", sprintf("%.2fM", n / 1e6)),
    n >= 1e3 ~ sub("\\.0K$", "K", sprintf("%.2fK", n / 1e3)),
    TRUE ~ as.character(n)
  )
}

#==================================#
# 3. EDA
# 3.1 Homepage
# Summary (Group_by --> Filter last year & first year)
#==================================#

#----------------------------------#
# HELPER FUNCTIONS
#----------------------------------#

pct_vs_baseline <- function(df, year, baseline, col) {
  n_year <- df |>
    filter(YEAR == year) |>
    pull({{col}})
  
  n_base <- df |>
    filter(YEAR == baseline) |>
    pull({{col}})
  
  round((n_year - n_base) / n_base * 100, 2)
}

get_operated_flights <- function(df){
  df |> filter(CANCELLED == 0 & DIVERTED == 0)
}

count_by_year <- function(df, condition = NULL, col_name = "n") {
  condition <- enquo(condition)
  
  if (!quo_is_null(condition)) {
    df <- filter(df, !!condition)
  }
  
  df |>
    group_by(YEAR) |>
    summarise(
      !!col_name := n(),
      .groups = "drop"
    ) |>
    arrange(YEAR)
}

ontime_delay_by <- function(df, group_cols){
  df_op <- get_operated_flights(df)
  
  df_op |>
    mutate(
      n_ontime = as.integer(DEP_DELAY <= 15),
      n_delayed = as.integer(DEP_DELAY > 15),
    ) |>
    group_by(across(all_of(group_cols))) |>
    summarise(
      n_ontime = sum(n_ontime, na.rm = TRUE),
      n_delayed = sum(n_delayed, na.rm = TRUE),
      n_operated = n(),
      avg_delay = mean(DEP_DELAY, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_ontime  = n_ontime / n_operated * 100,
      pct_delayed = n_delayed / n_operated * 100
    ) |>
    arrange(across(all_of(group_cols)))
}

#----------------------------------#
# PRE-COMPUTED EDA METRICS
#----------------------------------#
flight_yearly <- count_by_year(df_flights, col_name="n_flights")

df_cancel <- count_by_year(
  df_flights,
  CANCELLED == 1,
  col_name = "n_cancelled"
)

df_divert <- count_by_year(
  df_flights,
  DIVERTED == 1,
  col_name = "n_diverted"
)

df_operated <- count_by_year(
  df_flights,
  CANCELLED == 0 & DIVERTED == 0,
  col_name = "n_operated"
)

#----------------------------------#
# METRIC TREND + ICON
#----------------------------------#

metric_trend <- function(value, label = "") {
  is_positive <- value >= 0
  
  color <- ifelse(is_positive, "#2E7D32", "#C62828")  # xanh / đỏ
  arrow <- ifelse(is_positive, "▲", "▼")
  
  htmltools::tagList(
    # Label (KHÔNG đổi màu)
    htmltools::tags$span(
      style = "font-weight: 500; margin-right: 6px;",
      label
    ),
    
    # Value (CÓ màu)
    htmltools::tags$span(
      style = paste0(
        "color:", color,
        "; font-weight:600;"
      ),
      paste0(arrow, " ", abs(value), "%")
    )
  )
}

#----------------------------------#
df_ontime_delay <- ontime_delay_by(df_flights, "YEAR")

pct_23_19_flight <- pct_vs_baseline(flight_yearly, 2023, 2019, n_flights)

pct_23_19_cancel <- pct_vs_baseline(df_cancel, 2023, 2019, n_cancelled)
pct_23_19_divert <- pct_vs_baseline(df_divert, 2023, 2019, n_diverted)
pct_23_19_operated <- pct_vs_baseline(df_operated, 2023, 2019, n_operated)

pct_23_19_ontime <- pct_vs_baseline(df_ontime_delay, 2023, 2019, n_ontime)
pct_23_19_delayed <- pct_vs_baseline(df_ontime_delay, 2023, 2019, n_delayed)

total_flights <- nrow(df_flights)
total_airlines <- n_distinct(df_flights$AIRLINE)
total_airports <- n_distinct(df_flights$ORIGIN)
total_routes <- nrow(df_flights |>
                       distinct(ORIGIN, DEST))

total_flights_fmt <- format_compact(total_flights)

#==================================#
# Chart: Airline Rankings
#==================================#

df_air_rank <- df_flights |>
  group_by(AIRLINE) |>
  summarise(n_flights = n(), .groups = "drop") |>
  arrange(desc(n_flights)) |>
  slice_head(n = 14) |>   # ✅ TOP 10
  mutate(
    # ép factor trước khi plot để reorder
    AIRLINE = factor(AIRLINE, levels = AIRLINE),
    text = sapply(n_flights, format_compact)
  )

fig_airline_rank <- plot_ly(
  data = df_air_rank,
  x = ~n_flights,
  y = ~AIRLINE,
  type = "bar",
  orientation = "h",
  # ===== GRADIENT COLOR =====
  marker = list(
    color = ~n_flights,
    colorscale = list(
      list(0, "#99f6e4"),
      list(0.5, "#2dd4bf"),
      list(1, "#0f766e")
    ),
    showscale = FALSE
  ),
  hovertemplate = "<b>%{y}</b><br>%{customdata} flights<extra></extra>",
  customdata = df_air_rank$text   # số đã format (3.2M)
) |> 
  layout(
    # ===== TRỤC Y =====
    yaxis = list(
      autorange = "reversed",
      title = "",        # ẩn title
      showticklabels = TRUE, # vẫn hiện airline
      ticks = "",           # ẩn tick
      showline = FALSE,
      zeroline = FALSE
    ),
    # ===== TRỤC X =====
    xaxis = list(
      title = "",        # ẩn title
      showticklabels = FALSE, # ẩn số
      ticks = "",             # ẩn tick
      showgrid = FALSE,       # ẩn grid
      zeroline = FALSE,
      showline = FALSE
    ),
    title = list(
      text = paste0(
        "AIRLINE RANKING",
        "<br><span style='font-size:14px; color:#666;font-style:italic;'>Flight Counts</span>"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(size = 16, color = "#333", family = "Comic Sans MS")
    ),
    margin = list(l = 0, r = 0, t = 80, b = 0),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
  config(responsive = TRUE,
         displayModeBar = FALSE)

#==================================#
# Chart: Flight Distance Distribution
#==================================#

df_distance_departures <- df_flights |>
  group_by(DISTANCE_CAT) |>
  summarise(n_flights = n(), .groups = "drop") |>
  arrange(DISTANCE_CAT)

fig_distance_dist <- plot_ly(
  data = df_distance_departures
) |>
  add_bars(
    x = ~DISTANCE_CAT,
    y = ~n_flights,
    color = ~DISTANCE_CAT,
    colors = c("#F6C453", "#6EDBC4", "#F58BB6"),
    opacity = 0.8
  ) |>
  layout(
    showlegend = FALSE,
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    title = list(
      text = paste0(
        "FLIGHT DISTANCE DISTRIBUTION",
        "<br><span style='font-size:14px;color:#666;font-style:italic'>",
        "by number of flights</span>"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(size = 16, color = "#333", family = "Comic Sans MS")
    ),
    margin = list(l = 10, r = 10, t = 70, b = 10),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
  config(responsive = TRUE,
         displayModeBar = FALSE)

#==================================#
# Chart: National Coverage
#==================================#
df_flights <- df_flights |>
  mutate(
    ORIGIN_AIRPORT = paste0(ORIGIN_CITY, " (", ORIGIN, ")")
  )

df_origin_airport <- df_flights |>
  filter(!is.na(ORIGIN_LAT) & !is.na(ORIGIN_LON)) |>   # loại bỏ dòng NA
  group_by(ORIGIN_AIRPORT, ORIGIN_LAT, ORIGIN_LON) |>
  summarise(n_flights = n(), .groups = "drop") |>
  mutate(text = sapply(n_flights, format_compact))

pal <- colorNumeric(
  palette = c("#3b82f6", "#22c55e", "#f59e0b", "#ef4444"),
  domain = df_origin_airport$n_flights
)

radius_fun <- function(x) pmin(5, log(x + 1) * 4)

fig_national_coverage <- leaflet(df_origin_airport) |>
  # map background
  addProviderTiles(providers$CartoDB.Positron) |>
  setView(
    lng = mean(df_origin_airport$ORIGIN_LON),
    lat = mean(df_origin_airport$ORIGIN_LAT),
    zoom = 4
  ) |>
  addControl(
    html = "
      <div style='
        font-size:18px;
        font-weight:600;
        color:#333;
        padding:6px 10px;
      '>
        National Coverage
        <div style='font-size:13px; color:#666; font-style:italic;'>
          Number of flights by origin airport
        </div>
      </div>
    ",
    position = "topright"
  ) |>
  
  # 🔵 layer 1: halo mờ (nhòe)
  addCircleMarkers(
    lng = ~ORIGIN_LON,
    lat = ~ORIGIN_LAT,
    radius = ~radius_fun(n_flights) * 1.8,
    fillColor = ~pal(n_flights),
    fillOpacity = 0.25,
    stroke = FALSE
  ) |>
  
  # 🔴 layer 2: bubble chính (rõ)
  addCircleMarkers(
    lng = ~ORIGIN_LON,
    lat = ~ORIGIN_LAT,
    radius = ~radius_fun(n_flights),
    fillColor = ~pal(n_flights),
    fillOpacity = 0.75,
    stroke = FALSE,
    label = ~paste0(ORIGIN_AIRPORT, " - ", text, " flights")
  )

#==================================#
# Chart: Weekly Distribution
#==================================#

dow_map <- c(
  "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday",
  "4" = "Thursday", "5" = "Friday",
  "6" = "Saturday", "7" = "Sunday"
)

df_dow <- df_flights |>
  group_by(YEAR, DAY_OF_WEEK) |>
  summarise(n_flights = n(), .groups = "drop") |>
  mutate(
    DAY = factor(dow_map[as.character(DAY_OF_WEEK)],
                 levels = c("Monday","Tuesday","Wednesday","Thursday",
                            "Friday","Saturday","Sunday")),
    YEAR = as.character(YEAR)
  ) |>
  arrange(YEAR, DAY_OF_WEEK)

year_colors <- c(
  "2019" = "#F6C453",  # vàng
  "2020" = "#6EDBC4",  # xanh mint
  "2021" = "#F58BB6",  # hồng
  "2022" = "#a995ed",  # tím
  "2023" = "#ed8f6d"   # cam/coral
)


fig_dow_dist <- plot_ly(
  df_dow,
  x = ~DAY, 
  y = ~n_flights,
  color = ~YEAR, 
  colors = year_colors,
  type = "bar",
  opacity = 0.8,
  hovertemplate = paste(
    "<b>Day:</b> %{x}<br>",
    "<b>Flights:</b> %{y}<br>",
    "<b>Year:</b> %{customdata}"
  ),
  customdata = ~YEAR
) |>
  layout(
    barmode = "stack",
    showlegend = FALSE,
    xaxis = list(title = ""),
    yaxis = list(title = "", showgrid = FALSE),
    title = list(
      text = paste0(
        "WEEKLY DISTRIBUTION",
        "<br><span style='font-size:14px;color:#666;font-style:italic'>",
        "Number of flights</span>"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(size = 16, color = "#333", family = "Comic Sans MS")
    ),
    margin = list(l = 10, r = 10, t = 70, b = 10),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
  config(responsive = TRUE,
         displayModeBar = FALSE)

#==================================#
# Chart: Monthly Departures
#==================================#

df_monthly_departures <- df_flights |>
  group_by(MONTH) |>
  summarise(n_flights = n(), .groups = "drop") |>
  arrange(MONTH) |>
  mutate(text = sapply(n_flights, format_compact))

fig_monthly_departures <- plot_ly(
  df_monthly_departures,
  x = ~MONTH,
  y = ~n_flights,
  type = "scatter",
  mode = "lines+markers+text",
  text = ~text,
  textposition = "top center",
  cliponaxis = FALSE,
  opacity = 0.8,
  line = list(color = "#3b82f6", width = 3),
  marker = list(size = 8),
  fill = "tozeroy",      # fill area dưới line
  fillcolor = "#f7d960", # màu mảng
  hovertemplate = paste(
    "<b>Month:</b> %{x}<br>",
    "<b>Flights:</b> %{y}<extra></extra>"
  ),
  height = 400
) |>
  layout(
    yaxis = list(title = ""),
    xaxis = list(
      title = "",
      type = "category"
    ),
    title = list(
      text = paste0(
        "MONTHLY DEPARTURES",
        "<br><span style='font-size:14px;color:#666;font-style:italic'>",
        "by number of flights</span>"
      ),
      x = 0.5,
      xanchor = "center",
      font = list(size = 16, color = "#333", family = "Comic Sans MS")
    ),
    margin = list(l = 10, r = 10, t = 70, b = 10),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  ) |>
  config(responsive = TRUE, 
         displayModeBar = FALSE)

#==================================#
# 3.2 Operational Performance
# 3.2.1 Overview + With/Without Filter
#----------------------------------#
# On-time rate = Number of on-time flights / Total number of operated flights * 100
# Delay rate = Number of delayed flights / Total number of operated flights * 100
#----------------------------------#
# This is often simply "100% - Ontime Rate - Canceled Rate - Divert Rate",
# as every flight falls into one of these categories. The average delay time in
# minutes is another common metric.
#----------------------------------#
# Note: Canceled and diverted flights are typically excluded from the on-time
# performance calculation but included in the total pool of flight for overall performance metrics.
#----------------------------------#
# Cancelled rate = Number of canceled flights / Total number of scheduled flights * 100
# Divert rate = Number of diverted flights / Total number of scheduled flights * 100
#==================================#

filter_by_year <- function(df, year) {
  if (!is.null(year)) {
    df <- filter(df, YEAR == year)
  }
  return(df)
}

filter_by_airline <- function(df, airline) {
  if (!is.null(airline)) {
    df <- filter(df, AIRLINE == airline)
  }
  return(df)
}

filter_by_origin_airport <- function(df, airport) {
  if (!is.null(airport)) {
    df <- filter(df, ORIGIN == airport)
  }
  return(df)
}

filter_by_season <- function(df, season) {
  if (!is.null(season)) {
    df <- filter(df, SEASON == season)
  }
  return(df)
}

#==================================#
# On-time Performance & Delays
#==================================#
library(data.table)

# dt <- as.data.table(df_flights)
# 
# summary_table <- dt[
#   , .(
#     total_flights = .N,
#     cancelled     = sum(CANCELLED == 1),
#     diverted      = sum(DIVERTED == 1),
#     on_time       = sum(
#       DEP_DELAY <= 15 &
#         CANCELLED == 0 &
#         DIVERTED == 0
#     )
#   ),
#   by = .(YEAR, AIRLINE)
# ]
# 
# summary_table[
#   , operated := total_flights - cancelled - diverted
# ][
#   , `:=`(
#     on_time_rate = round(on_time / operated * 100, 2),
#     delay_rate   = round((operated - on_time) / operated * 100, 2)
#   )
# ]
# 
# ontime_delay_summary <- function(year = "All", airline = "All") {
#   
#   dt <- summary_table
#   
#   if (year != "All") {
#     dt <- dt[YEAR == year]
#   }
#   
#   if (airline != "All") {
#     dt <- dt[AIRLINE == airline]
#   }
#   
#   if (nrow(dt) == 0 || sum(dt$operated) == 0) {
#     return(list(
#       total_flights = 0,
#       on_time_rate  = 0,
#       delay_rate    = 0
#     ))
#   }
#   
#   list(
#     total_flights = sum(dt$total_flights),
#     on_time_rate  = round(sum(dt$on_time) / sum(dt$operated) * 100, 2),
#     delay_rate    = round(
#       (sum(dt$operated) - sum(dt$on_time)) / sum(dt$operated) * 100, 2
#     )
#   )
# }

filter_by_year <- function(df, year) {
  if (!is.null(year)) {
    df <- filter(df, YEAR == year)
  }
  return(df)
}

filter_by_airline <- function(df, airline) {
  if (!is.null(airline)) {
    df <- filter(df, AIRLINE == airline)
  }
  return(df)
}

filter_by_origin_airport <- function(df, airport) {
  if (!is.null(airport)) {
    df <- filter(df, ORIGIN == airport)
  }
  return(df)
}

filter_by_season <- function(df, season) {
  if (!is.null(season)) {
    df <- filter(df, SEASON == season)
  }
  return(df)
}

#==================================#
# On-time Performance & Delays
#==================================#
ontime_delay_summary <- function(df, year = NULL, airline = NULL) {
  
  if (!is.null(year) && year != "All") {
    df <- filter_by_year(df, year)
  }
  
  if (!is.null(airline) && airline != "All") {
    df <- filter_by_airline(df, airline)
  }
  
  total_flights <- nrow(df)
  
  cancelled <- df$CANCELLED == 1
  diverted  <- df$DIVERTED  == 1
  operated  <- !cancelled & !diverted
  on_time   <- operated & df$DEP_DELAY <= 15
  
  total_cancelled <- sum(cancelled)
  total_diverted  <- sum(diverted)
  total_operated  <- sum(operated)
  total_on_time   <- sum(on_time)
  total_delayed   <- total_operated - total_on_time
  
  list(
    total_flights = total_flights,
    on_time_rate  = round(total_on_time / total_operated * 100, 2),
    delay_rate    = round(total_delayed / total_operated * 100, 2)
  )
}




















