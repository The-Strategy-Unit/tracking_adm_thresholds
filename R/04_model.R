# README
# Fit models for each provider site, in parallel. 
# Calculate model performance metrics and capture model results.

# rm(df_ecds_joined)
# rm(df_var_engineering_1of2)
# rm(df_providers_nested)
# rm(df_providers_nested_specs)
rm(df_ecds_raw)

gc()
gc()
gc()

df_prov_low_admirate <- df_var_engineering_2of2 |>
  mutate(p_adm = map_dbl(data, function(df) {
    df |>
      count(admitted) |>
      mutate(p = n/sum(n)) |>
      filter(admitted == 1) |> 
      pull(p)
  }
  )) |>
  # mutate(p_adm = abs(p)) |>
  # mutate(site_name = map_chr(data, \(df) df |> distinct(site_name) |> pull() |> as.character())) |>
  distinct(der_provider_site_code, ed_name, p_adm) |>
  arrange(p_adm) |>
  filter(p_adm <0.05)
  # print(n= 20)
  # view("p_adm")
  # identity()



# SAMPLE -------------------------------------------------------------------------

# df_model_test <- df_var_engineering_2of2 |> 
#   inner_join(df_mixed, join_by(der_provider_site_code == value)) |>
#   mutate(ed_name = map_chr(data, \(df) df |> distinct(site_name) |> pull() |> as.character())) |> 
#   # count(dq, der_provider_site_code, ed_name) |> 
#   identity()


# END SAMPLE ----------------------------------------------------------------

plan(multisession, workers = 4)
future::futureSessionInfo()

# TODO IS FUTURE MAP2 VIABLE (TOO SLOW VS FUTURE MAP?)
tictoc::tic()
# df_models <- df_var_engineering_2of2 |>
set.seed(1001)
df_models <-
  df_var_engineering_2of2 |>
  ### OPTION FOR DEMO:
  anti_join(df_prov_low_admirate, join_by(der_provider_site_code)) |> 
  filter(
    der_provider_site_code %in% sample(vec_providers_diag, 2) |
      der_provider_site_code %in% vec_providers_diag_inconstant |
      der_provider_site_code %in% sample(vec_providers_complaint_candidates, 2) |
      str_detect(ed_name, "Hull") |
      str_detect(ed_name, "St George")
  ) |> 
  ### TAKE ONE FROM BETTER GROUP, ONE FROM WORSE:
  # slice(c(1:2, 357:358)) |> 
  # filter(der_provider_site_code %in% c(
  #   "RWA01", # Hull Royal Infirmary
  #   "RQWG0"  # The Princess Alexandra Hospital
  #   )
  #   )|>
  ###
  ###
  mutate(mod = future_map2(data, mod_formula, function(df, x) {
    mgcv::gam(
      formula = as.formula(x),
      # formula = admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + wkend + night_time_8to8 + diag01 + arrmode_ambulance + acuity + referral_source,
      family = "binomial",
      data = df
    ) 
  }))
plan(sequential)
gc()
gc()
gc()
tictoc::toc()

# APPROX 375 secs for 4 models, 4 workers 
# APPROX 800 secs for 16 models, 4 workers 

# 7. METRICS AND RELATIONSHIP TO DQ --------------------------------

df_models |> 
  # TODO NOT SURE BRIER SCORE IS CORRECT:
  mutate(brier = map2(data, mod, function(df, x) {
    tibble(
      obs = df$admitted,
      # fit = 1-fitted(x), # predicted probabilities 1
      fit = fitted(x), # predicted probabilities 1
    ) |>
      brier_class(obs, fit) 
    })) |>
  mutate(roc = map2(data, mod, function(df, x) {
    tibble(
      obs = df$admitted,
      fit = fitted(x), # predicted probabilities 1
    ) |>
      roc_auc(obs, fit, event_level = "second") 
  })) |>
  pivot_longer(cols = c(brier, roc), names_to = "met") |> 
  unnest(value) |> 
  select(-c(mod_formula, data, mod, met, .estimator)) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  # identity()
  left_join(
    df_provider_dataqual |>
      select(1,2,4), 
    join_by(der_provider_site_code)
    ) |> 
  left_join(
    df_provider_dataqual_mean |>
      select(1,2,4),
    join_by(der_provider_site_code)
    ) |> 
  filter(mod_spec %in% c("a", "b"))


# TABLE OF COEFFICIENTS -----------------------------------------------------------------

# df_models$mod[[3]] 
  
df_coeff <- df_models |> 
  mutate(mod_coeffs = map(mod, function(m) {
  m |> 
  tidy(parametric = TRUE) |> 
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96*std.error)) |> 
  mutate(uci = exp(estimate + 1.96*std.error)) |> 
  # select(term, raw_estimate = estimate, odds, lci, uci, p.value) |> 
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value >= 0.001 &  p.value < 0.01 ~ "**",
    p.value >= 0.01 &  p.value < 0.05 ~ "*",
    p.value >= 0.05 &  p.value <= 0.1 ~ ".",
    T ~ " "
  )) |> 
  mutate(across(where(is.numeric), ~ round(., digits = 4))) 

  }))

# df_coeff$mod_coeffs[[1]]
