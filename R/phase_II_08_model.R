# README
# Fit the model to a data sample of X million.

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

# 1m rows = 26 mins


# df_var_ref_levels |>
#     arrow::write_parquet(
#       sink = here("data", "251203_df_var_ref_levels.gzip.parquet"),
#       compression = "gzip"
#     )

df_var_ref_levels <- arrow::open_dataset(here::here("data", "251203_df_var_ref_levels.gzip.parquet")) |>
  collect()

# TODO THIS - OR WHATEVER THE SOLUTION IS - SHOULD GO IN DATA PREP:
df_var_ref_levels <- df_var_ref_levels |> 
  mutate(acuity = as.character(acuity)) |> 
  mutate(acuity = case_when(
    site_name == "Great Western Hospital A&E" &
      acuity %in% c("Standard", "Urgent", "Non-urgent") ~ "NonUrg/Std/Urg",
    site_name == "Worthing Hospital" &
      acuity %in% c("Standard", "Non-urgent") ~ "NonUrg/Std",
    site_name == "Leicester Royal Infirmary" &
      acuity %in% c("Standard", "Urgent") ~ "Std/Urg",
    T ~ acuity
  )) |> 
  mutate(acuity = fct_relevel(acuity, "Standard"))
  

# df_var_ref_levels %>%
#   # colnames(df_ecds_II_raw)[c()] %>%
#   colnames() %>%
#   map(~ count(df_var_ref_levels, is.na(.data[[.x]]), sort = T))

set.seed(1001)
df_var_ref_levels_sample_1m <- df_var_ref_levels |>
  slice_sample(n = 1e6)


mod_run_2 <-
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

mod_run_2 |> saveRDS(here::here("data", "251205_mod_run_2_acuity.rds"))
