source(here("R", "funs.R"))

# A. SPECIFY MODEL FORMULA BASED ON DATA QUAL ---------------------------

# A PROVIDER'S LOWEST DATA COMPLETENESS VALUE (FOR KEY VARIABLES) FOR A WEEK WITHIN STUDY PERIOD: 
df_provider_dataqual <- return_completion_min_week(der_ec_diagnosis_all) |> 
  left_join(return_completion_min_week(ec_acuity_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_min_week(ec_chief_complaint_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_min_week(ec_arrival_mode_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_min_week(ec_attendance_source_snomed_ct), join_by(der_provider_site_code)) |> 
  rename(
    p_complnt = p_chief_complaint,
    p_arrmode = p_arrival_mode,
    p_refsorc = p_attendance_source
  ) |> 
  # IF THERE ARE WEEKS WITH ALL NAs FOR A VARIABLE, THEN SET % TO ZERO:
  mutate(across(starts_with("p_"), ~ if_else(is.na(.), 0, .)))

df_provider_dataqual_mean <- return_completion_overall(der_ec_diagnosis_all) |> 
  left_join(return_completion_overall(ec_acuity_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_overall(ec_chief_complaint_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_overall(ec_arrival_mode_snomed_ct), join_by(der_provider_site_code)) |> 
  left_join(return_completion_overall(ec_attendance_source_snomed_ct), join_by(der_provider_site_code)) |> 
  rename(
    p_complnt = p_chief_complaint,
    p_arrmode = p_arrival_mode,
    p_refsorc = p_attendance_source
  ) |> 
  # IF THERE ARE WEEKS WITH ALL NAs FOR A VARIABLE, THEN SET % TO ZERO:
  mutate(across(starts_with("p_"), ~ if_else(is.na(.), 0, .)))


df_provider_dataqual_sdev_prep <- 
  return_completion_all_weeks(der_ec_diagnosis_all) |> 
  left_join(
    return_completion_all_weeks(ec_chief_complaint_snomed_ct),
    join_by(der_provider_site_code, year_week)
  ) |> 
  rename(
    p_complnt = p_chief_complaint
  ) |> 
  # IF THERE ARE WEEKS WITH ALL NAs FOR A VARIABLE, THEN SET % TO ZERO:
  mutate(across(starts_with("p_"), ~ if_else(is.na(.), 0, .))) |> 
  select(-c(contains("valid"), n.x, n.y))
  
df_provider_dataqual_sdev <- df_provider_dataqual_sdev_prep |> 
  group_by(der_provider_site_code) |> 
  summarise(sd_diag = sd(p_diag), sd_complnt = sd(p_complnt), n_wks = n())

gc()
gc()
