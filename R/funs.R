# README
# Functions used in admission thresholds tracker project.
library("lubridate")
library("dplyr")


# Calculate the percentage of na values for a specified field, weekly,
# for each provider in the dataset:

calc_perc_na_weekly <- function(x) {
  df_ecds_raw |> 
    # BE FORGIVING OF QUALITY IN LAST 3 WEEKS BY CUTTING THESE.
    # (ANOMALIES SHOULD BE PICKED UP IN GRAPHICS)
    # TODO: MAYBE ADD NOTE ABOUT 3 WEEKS AND LESS IS PERHAPS NOT RECOMMENDED.
    filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(21)) |>
    # filter(der_ec_arrival_date_time <= as_date(Sys.time()) - days(45)) |>
    count(
      der_provider_code,
      year_week = yearweek(der_ec_arrival_date_time),
      valid = !is.na({{x}})
      ) |> 
    group_by(der_provider_code, year_week) |> 
    mutate(p = n/sum(n)) |> 
    ungroup() |> 
    filter(valid == TRUE) |> 
    group_by(der_provider_code) |> 
    filter(p == min(p)) |> 
    ungroup() |>
    distinct(der_provider_code, p) |> 
    mutate("p_{{x}}" := p, .keep = "unused") |> 
    rename_with( 
      ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
      starts_with("p_ec_") | starts_with("p_der_ec")
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

# ARCHIVED -----------------------------------------------------------

# # Calculate the percentage of na values for a specified field, 
# # for each provider in the dataset:
# calc_perc_na <- function(x) {
#   df_ecds_raw |> 
#     count(der_provider_code, valid = !is.na({{x}})) |> 
#     group_by(der_provider_code) |> 
#     mutate("p_{{x}}" := n/sum(n)) |> 
#     ungroup() |> 
#     filter(valid == TRUE) |> 
#     rename_with( 
#       ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
#       starts_with("p_ec_") | starts_with("p_der_ec")
#     ) |> 
#     select(-c(valid, n))
# }