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












