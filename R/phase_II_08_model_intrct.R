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
#       sink = here("data", "251211_df_var_ref_levels.gzip.parquet"),
#       compression = "gzip"
#     )

df_var_ref_levels <- arrow::open_dataset(here::here("data", "251211_df_var_ref_levels.gzip.parquet")) |>
  collect() |> 
  mutate(na_diag = if_else(diag == "l2_na_complaint", 1, 0)) |>
  mutate(na_diag = as.factor(na_diag)) |>
  identity()

set.seed(1988)
df_var_ref_levels_sample <- df_var_ref_levels |>
  slice_sample(n = 2.5e6)


# _4. ACUITY MERGED, COVID V2 ADDED, NEW REF LEVEL FOR QUARTER, INTERACTION(S)
mod_run_9 <-
  mgcv::gam(
    formula = admitted ~ year_quarter +
      s(age, by = sex) + sex + imd_quint + ethnic_grp_sus +
      wkend + night_time_8to8 +
      diag + arrmode_ambulance + referral_source +
      # INTERACTION IN PLACE OF RANDOM INTERCEPT:
      acuity*site_name,
      # # ADDITIONAL INTERACTION:
      # na_diag + na_diag:site_name,
      # SITE (RANDOM INTERCEPT):
      # s(site_name, bs = "re"),
    family = "binomial",
    method = "REML",
    data = df_var_ref_levels_sample
  )

mod_run_9 |> saveRDS(here::here("data", "251215_mod_run_9_interaction.rds"))
