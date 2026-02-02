# README
# Fit the model to a data sample of X million.
# Cab be run as background job.

library("here")
library("mgcv")
library("broom")
library("dplyr")
library("readr")
library("tidyr")
library("tibble")
library("forcats")
library("ggplot2")
library("janitor")
library("stringr")
library("tsibble")
library("lubridate")
library("yardstick")

options(scipen=999) 

# 1m rows ~ 26 mins


# df_var_ref_levels |>
#     arrow::write_parquet(
#       sink = here("data", "251208_df_var_ref_levels.gzip.parquet"),
#       compression = "gzip"
#     )

df_var_ref_levels <- arrow::open_dataset(here::here("data", "251208_df_var_ref_levels.gzip.parquet")) |>
  collect()

set.seed(1001)
df_var_ref_levels_sample_1m <- df_var_ref_levels |>
  slice_sample(n = 1e6)


# _4. ACUITY MERGED, COVID ADDED, NEW REF LEVEL FOR QUARTER
mod_run_6 <-
  mgcv::gam(
    formula = admitted ~ year_quarter +
      s(age, by = sex) + sex + imd_quint + ethnic_grp_sus +
      wkend + night_time_8to8 +
      diag + arrmode_ambulance + acuity + referral_source +
      # SITE (RANDOM INTERCEPT):
      s(site_name, bs = "re"),
    family = "binomial",
    method = "REML",
    data = df_var_ref_levels_sample_1m
  )

mod_run_6 |> saveRDS(here::here("data", "251208_mod_run_6_covid_v2.rds"))
