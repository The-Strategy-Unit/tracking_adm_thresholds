# README 
# Some optional code to split providers into two groups - 
# Group 1: data quality likely not problematic
# Group 2: data quality possibly problematic.

# WORKING CRITERIA FOR MODEL SPEC SELECTION:
# IF 
#   MIN DIAG BELOW 40% 
#   AND MIN COMPLAINT IS 50% HIGHER THAN MIN DIAG VALUE
# THEN TAKE COMPLAINT
# ELSE DIAG.

tmp_dq <- df_provider_dataqual |> 
  select(der_provider_site_code, contains("diag"), contains("complnt")) |>
  arrange(p_diag) |>
  mutate(d = p_complnt - p_diag) |> 
  # filter(d >= .5)
  mutate(flag = if_else(d >= 0.5, 1, 0)) |> 
  left_join(
    df_provider_dataqual_mean |> 
      select(der_provider_site_code, contains("diag"), contains("complnt")),
    join_by(der_provider_site_code)
  ) |> 
  left_join(df_provider_dataqual_sdev, join_by(der_provider_site_code)) |> 
  rename(
    min_diag = p_diag.x, 
    min_comp = p_complnt.x, 
    delta_min = d,
    mn_diag = p_diag.y,
    mn_comp = p_complnt.y
    
  ) |> 
  arrange(-sd_diag) |>
  # filter(flag == 1) |> 
  # select(der_provider_site_code, contains("comp")) |> 
  # arrange(sd_diag) |> 
  # print(n=25) |> 
  identity()

vec_providers_good_diag <- tmp_dq |> 
  arrange(desc(min_diag)) |> 
  slice(1:6) |> 
  pull(der_provider_site_code)

vec_providers_complaint_candidates <- tmp_dq |> 
  filter(flag == 1 ) |> 
  pull(der_provider_site_code)

vec_providers_bad_diag <- tmp_dq |> 
  filter(min_diag < 0.4) |> 
  pull(der_provider_site_code)

vec_providers_missing_weeks <- df_provider_dataqual_sdev |>
  filter(n_wks < max(n_wks)) |>
  count(der_provider_site_code, n_wks) |>
  arrange(n_wks) |>
  pull(der_provider_site_code)

vec_shaky_providers <- unique(
  c(
    # vec_providers_missing_weeks,
    vec_providers_bad_diag,
    vec_providers_complaint_candidates
  )
)
