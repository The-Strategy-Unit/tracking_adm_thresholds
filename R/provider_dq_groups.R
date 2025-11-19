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

# TODO NOTE ALSO THOSE WITH NO / V. LOW ADM RATE


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


vec_providers_complaint_candidates <- tmp_dq |> 
  filter(flag == 1 ) |> 
  pull(der_provider_site_code)

# PROVIDERS WITH HIGH SD IN WEEKLY DIAGNOSIS RATES:
vec_providers_diag_inconstant <- tmp_dq |> 
  filter(flag == 0) |> 
  slice(c(1,4)) |> 
  pull(der_provider_site_code)

vec_providers_diag <-
  setdiff(
    setdiff(
      tmp_dq |> pull(der_provider_site_code),
      vec_providers_complaint_candidates
    ),
    vec_providers_diag_inconstant
  )



# # PERHAPS REMOVE THESE
# OR LET THE INTERACTION GRAPH DO THE WORK
# vec_providers_low_diag <- tmp_dq |> 
#   print(n=30)
#   filter(sd_diag > ...) |> 
#   pull(der_provider_site_code)

# setdiff(vec_providers_low_diag, vec_providers_complaint_candidates)


# PROVIDERS WITH INCOMPLETE TIMESERIES: (SEE MISSING WEEKS BELOW)
# df_provider_dataqual_sdev_prep |> 
#   count(der_provider_site_code) |> 
#   arrange(n)

# vec_providers_missing_weeks <- df_provider_dataqual_sdev |>
#   filter(n_wks < max(n_wks)) |>
#   count(der_provider_site_code, n_wks) |>
#   arrange(n_wks) |>
#   pull(der_provider_site_code)


# vec_shaky_providers <- unique(
#   c(
#     # vec_providers_missing_weeks,
#     vec_providers_bad_diag,
#     vec_providers_complaint_candidates
#   )
# )
