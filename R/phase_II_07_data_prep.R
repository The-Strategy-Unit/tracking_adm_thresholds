# 1. ADD DIAG (VARIABLE TO INDICATE CONDITION) --------------------------

df_add_diag_prep <- df_ecds_II_sample |>
  left_join(lkp_diag_II, join_by(diag01_code))

df_add_diag_l3 <- df_add_diag_prep |>
  filter(!is.na(diag))


df_add_diag_l2_l1 <- df_add_diag_prep |>
  filter(is.na(diag)) |>
  select(-diag) |>
  left_join(lkp_complaint_II, join_by(ec_chief_complaint_snomed_ct))

df_diag_added <- bind_rows(
  df_add_diag_l3,
  df_add_diag_l2_l1
) |>
  mutate(diag = fct_relevel(diag, "l3_lower_respiratory_tract_infection"))

# df_diag_added |>
#   count(diag, sort = T) |>
#   print(n=67)


# 2.  ---------------------------------------------------------------------

df_var_engineering <- df_diag_added |> 
  mutate(acuity = str_remove_all(ref_acuity, " level emergency care")) |>
  mutate(acuity = if_else(is.na(acuity), "NA", acuity)) |>
  mutate(referral_source = case_when(
    ref_attsrc %in% c("Referred by self", "Self-referral to accident and emergency department") ~ "self",
    ref_attsrc == "Referred by ambulance service" ~ "ambulance",
    ref_attsrc == "Referred by NHS 111 service" ~ "111",
    ref_attsrc == "Referred by member of Primary Health Care Team" ~ "primary_care",
    ref_attsrc %in% c( 
      "Referred by mental health assessment team",
      "Referred by community mental health nurse",
      "Referred by urgent treatment centre",
      "Referred by out of hours service",
      "Referred by hospital emergency department",
      "Referred by hospital outpatient department",
      "Referred by advanced care practitioner",
      "Referred by community nurse",
      "Referred by hospital ward"
    ) ~ "other_hcp",
    is.na(ref_attsrc) ~ "other",
    TRUE ~ "other"
  )) |>
  # ALL AMBULANCES:
  mutate(arrmode_ambulance = case_when(
    ref_arrmode %in% c(
      "Arrival by emergency road ambulance",
      "Arrival by emergency road ambulance with medical escort",
      "Arrival by helicopter air ambulance",
      "Arrival by medical repatriation air ambulance",
      "Arrival by non-emergency road ambulance"
    ) ~ "1",
    is.na(ref_arrmode) ~ "0",
    TRUE ~ "0"
  )) |> 
  left_join(lkp_ethref_raw, join_by(ethnic_category == code)) |>
  mutate(ethnic_grp_sus = if_else(is.na(ethnic_grp_sus), "NA", ethnic_grp_sus)) |> 
  mutate(imd_quint = as.character(
    round_half_up(
      as.numeric(index_of_multiple_deprivation_decile) / 2
    )
  )) |>
  mutate(imd_quint = if_else(is.na(imd_quint), "NA", imd_quint)) |>
  rename(age = der_age_at_cds_activity_date) |>
  # mutate(year_month = yearmonth(der_ec_arrival_date_time)) |>
  # mutate(year_week = yearweek(der_ec_arrival_date_time)) |>
  mutate(wkend = if_else(
    wday(der_ec_arrival_date_time, week_start = 1) %in% 6:7,
    1, 0
  )) |>
  mutate(night_time_8to8 = if_else(
    hour(der_ec_arrival_date_time) %in% 8:19,
    0, 1
  )) 


# 3. ----------------------------------------------------------------------

df_var_ref_levels <- df_var_engineering |>
  select(
    admitted, site_name, year_quarter,
    age, sex, imd_quint, ethnic_grp_sus,
    acuity, diag, arrmode_ambulance, referral_source, 
    night_time_8to8, wkend
  ) |> 
  ## EXCLUDING DIAG AS REF LEVEL ALREADY SPECIFIED:
  mutate(across(c(everything(), - age, -matches("diag$")), ~ as.factor(.))) |>
  # EXPLICITLY SET REF LEVEL FOR OUTCOME (NECESSARY FOR CONSISTENT METRICS):
  mutate(admitted = fct_relevel(admitted, "0")) |> 
  # SET REF LEVEL FOR OTHER CATS: (HIGH VOLUME, MODERATE EFFECT)
  mutate(acuity = fct_relevel(acuity, "Standard")) |> 
  mutate(ethnic_grp_sus = fct_relevel(ethnic_grp_sus, "British, Mixed British")) |> 
  # mutate(referral_source = fct_relevel(referral_source, "self")) |> 
  # SET REF CATEGORY FOR YEAR_MONTH (MOST RECENT QUARTER USED):
  mutate(year_quarter = fct_relevel(year_quarter, as.character(yearquarter("2025-02-01", fiscal_start = 4)))) |>
  identity()


gc()
gc()
gc()
gc()


# * EDA: ACUITY OVER TIME ------------------------------------------------

df_var_ref_levels |>
  count(site_name, year_quarter, acuity == "NA") |> 
  rename(x =3) |> 
  filter(x == T) |> 
  ggplot(aes(year_quarter, n))+
  geom_line(group = 1)+
  geom_point(size = .6)+
  geom_blank(aes(y=0))+
  facet_wrap(vars(site_name), scales = "free_y")+
  theme_minimal()+
  theme(
    legend.position = "top",
    axis.text = element_text(size = 5),
    axis.title = element_blank(),
    strip.text = element_text(size = 6),
  )

df_acuity_over_time <- df_var_ref_levels |>
  mutate(acui2 = as.character(acuity)) |> 
  mutate(acui2 = if_else(acuity %in% c("Standard", "Urgent"), "Standard/Urgent", acui2)) |> 
  mutate(acui3 = if_else(acuity %in% c("Standard", "Urgent", "Non-urgent"), "Standard/Urgent/Non", acui2)) |> 
  count(site_name, year_quarter, acuity, acui2, acui3) 
  
df_acuity_over_time |> saveRDS(here("data", "251204_dfq_acuity_over_time.rds"))

  
  # ggplot(aes(year_quarter, n, col = acuity, group = acuity))+
  count(site_name, year_quarter, acui2) |> 
  ggplot(aes(year_quarter, n, col = acui2, group = acui2))+
  geom_line()+
  geom_point(size = .6)+
  geom_blank(aes(y=0))+
  facet_wrap(vars(site_name), scales = "free_y")+
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    axis.text = element_text(size = 5),
    axis.title = element_blank(),
    strip.text = element_text(size = 6),
  )+
  guides(colour = guide_legend(nrow = 1))




