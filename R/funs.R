# README
# Functions used in admission thresholds tracker project.
library("lubridate")
library("dplyr")

# Cutoff date will be set to the Sunday before/of the minimum buffer period specified
# so that we deal only with complete weeks (Monday to Sunday)
set_cutoff_date <- function(min_buffer_days) {
  min_buffer_date <- as_date(Sys.time()) - days(min_buffer_days)

  min_buffer_weekday <- min_buffer_date |>
    wday(week_start = 1)

  return(
    if_else(
      min_buffer_weekday == 7,
      min_buffer_date - days(0),
      min_buffer_date - days(min_buffer_weekday)
    )
  )
}


# Function to get the first Monday of a given year:

get_first_monday <- function(year) {
  start_date <- make_date(year, 01, 01)

  weekday <- start_date |>
    wday(week_start = 1)

  start_date +
    if_else(weekday == 1, 0, 8 - weekday)
}


# Function to get the first Monday of a given financial year:

get_first_monday_fyear <- function(fyear) {
  start_date <- make_date(fyear, 04, 01)
  
  weekday <- start_date |>
    wday(week_start = 1)
  
  start_date +
    if_else(weekday == 1, 0, 8 - weekday)
}


# Provide the % records complete for a specified field, x,
# for the week with lowest completion (i.e. highest % na values) 
# for each provider in the dataset:
return_completion_min_week <- function(x) {
  df_ecds_raw |> 
    # BE FORGIVING OF QUALITY IN LAST 3 WEEKS BY CUTTING THESE.
    # (ANOMALIES SHOULD BE PICKED UP IN GRAPHICS)
    # TODO: MAYBE ADD NOTE ABOUT 3 WEEKS AND LESS IS PERHAPS NOT RECOMMENDED.
    filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(21)) |>
    # filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(45)) |>
    count(
      der_provider_site_code,
      year_week = yearweek(der_ec_arrival_date_time),
      valid = !is.na({{x}})
      ) |> 
    group_by(der_provider_site_code, year_week) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    filter(valid == TRUE) |> 
    group_by(der_provider_site_code) |> 
    filter(p == min(p)) |> 
    ungroup() |>
    distinct(der_provider_site_code, p) |> 
    mutate("p_{{x}}" := p, .keep = "unused") |> 
    rename_with( 
      ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
      starts_with("p_ec_") | starts_with("p_der_ec")
    ) 
}


# Provide the % records complete for a specified field, x,
# for each provider in the dataset:
return_completion_overall <- function(x) {
  df_ecds_raw |>
    filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(21)) |>
    count(der_provider_site_code, valid = !is.na({{x}})) |>
    group_by(der_provider_site_code) |>
    mutate("p_{{x}}" := n/sum(n)) |>
    ungroup() |>
    filter(valid == TRUE) |>
    rename_with(
      ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
      starts_with("p_ec_") | starts_with("p_der_ec")
    ) |>
    select(-c(valid, n))
}


# Provide the % records complete for a specified field, x,
# for all weeks, for each provider in the dataset:

return_completion_all_weeks <- function(x) {
  df_ecds_raw |> 
    # BE FORGIVING OF QUALITY IN LAST 3 WEEKS BY CUTTING THESE.
    # (ANOMALIES SHOULD BE PICKED UP IN GRAPHICS)
    # TODO: MAYBE ADD NOTE ABOUT 3 WEEKS AND LESS IS PERHAPS NOT RECOMMENDED.
    filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(21)) |>
    # filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(45)) |>
    count(
      der_provider_site_code,
      year_week = yearweek(der_ec_arrival_date_time),
      valid = !is.na({{x}})
    ) |> 
    group_by(der_provider_site_code, year_week) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    filter(valid == TRUE) |> 
    mutate("p_{{x}}" := p, .keep = "unused") |> 
    rename_with( 
      ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
      starts_with("p_ec_") | starts_with("p_der_ec")
    ) 
}


