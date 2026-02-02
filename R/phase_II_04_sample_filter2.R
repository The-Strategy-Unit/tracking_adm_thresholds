# README
# We have already broadly filtered out sites with low diagnosis completion.
# Here, though we look more closely at non-constant coding of diagnosis over time.
# We remove providers with wild swings in completion rates. 


df_ecds_II <- df_ecds_II_raw |> 
  semi_join(df_provider_sample_stage_1.5, join_by(der_provider_site_code)) |> 
  mutate(diag01_code = str_extract(der_ec_diagnosis_all, "^[^,]*")) |>
  ### JOIN TO REFERENCE DF FOR TEXT DESRIPTION OF SNOMED CODES:
  left_join(df_ref_trimmed, join_by(ec_arrival_mode_snomed_ct == snomed_code)) |>
  rename(ref_arrmode = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_attendance_source_snomed_ct == snomed_code)) |>
  rename(ref_attsrc = derived_snomed_descr) |>
  # left_join(df_ref_trimmed, join_by(ec_discharge_status_snomed_ct == snomed_code)) |>
  # rename(ref_disstat = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
  rename(ref_disdest = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_acuity_snomed_ct == snomed_code)) |>
  rename(ref_acuity = derived_snomed_descr)

gc()
gc()
gc()
gc()
gc()

# df_ecds_II |>
#   arrow::write_parquet(
#     sink = here("data_raw", "251128_ecds_II.gzip.parquet"),
#     compression = "gzip"
#   )
# 
df_ecds_II <- arrow::open_dataset(here::here("data_raw", "251128_ecds_II.gzip.parquet")) |>
  collect() |> 
  mutate(admitted = case_when(
    # # THESE WILL TRUMP EVERYTHING ELSE (BUT MINIMAL EFFECT FOR OUR SAMPLE): 
    ec_discharge_status_snomed_ct == "1077081000000104" ~ 0, # SDEC
    ec_discharge_status_snomed_ct == "1077031000000103" ~ 0, # UTC
    ec_discharge_status_snomed_ct == "1077021000000100" ~ 0, # GP
    # THEN
    ref_disdest %in% c(
      "Discharge to ward",
      "Emergency department discharge to coronary care unit",
      "Emergency department discharge to high dependency unit",
      "Emergency department discharge to intensive care unit",
      "Emergency department discharge to operating theatre",
      "Emergency department discharge to neonatal intensive care unit",
      "Emergency department discharge to special care baby unit"  
    ) ~ 1,
    # is.na(ref_disdest) & ec_discharge_status_snomed_ct == "1324201000000109" ~ 0,
    is.na(ref_disdest) ~ 0,
    TRUE ~ 0
  ))

gc()
gc()
gc()
gc()
gc()


# -------------------------------------------------------------------------


# PLOT OF GENERAL CODING OVER TIME ------------------------------------------------
# SOME PRE AND POST 2022 DIAGNOSIS CODES NEED TO BE TIED TOGETHER

df_diag_consistency <- df_ecds_II |>
  count(year_quarter, diag01_code)

df_freq_diagnoses <- df_diag_consistency |>
  arrange(year_quarter, -n) |>
  group_by(year_quarter) |>
  mutate(rank = min_rank(desc(n))) |>
  ungroup() |>
  filter(rank <= 30) |>
  distinct(diag01_code) |>
  left_join(df_ref_diagnosis, join_by(diag01_code == snomed_code))
# 
# eda_diag_consistency |> 
#   inner_join(eda_freq_conditions, join_by(diag01_code)) |> 
#   filter(as_date(year_quarter) < as_date("2025-07-01")) |> 
#   # filter(is.na(diag_descr_snomed))
#   mutate(diag_descr_snomed = if_else(diag01_code == "", "none", diag_descr_snomed)) |> 
#   ggplot(aes(as_date(year_quarter), n, group = diag_descr_snomed, col = diag_descr_snomed))+
#   geom_line()+
#   geom_point(size = 0.6)+
#   geom_blank(aes(y = 0))+
#   # facet_wrap(vars(diag_descr_snomed))+
#   facet_wrap(vars(diag_descr_snomed), scales = "free_y")+
#   theme_minimal()+
#   theme(
#     strip.text = element_text(size = 5.5),
#     legend.position = "none",
#     axis.text = element_text(size = 5),
#     axis.title = element_text(size = 5),
#     )

# UTIs and copds could be joined
# referral to gp, to service -  TO NA ???
# rise in NAs -- related to above? Provider specific?


# BY PROVIDER -------------------------------------------------------------

df_diag_consistency_provider <- df_ecds_II |> 
  count(der_provider_site_code, year_quarter, diag01_code)

df_consistency_preplot <- df_diag_consistency_provider |> 
  group_by(der_provider_site_code, year_quarter) |> 
  mutate(p = n/sum(n)) |> 
  ungroup() |> 
  inner_join(df_freq_diagnoses, join_by(diag01_code)) |>
  filter(as_date(year_quarter) < as_date("2025-07-01")) |> 
  # filter(is.na(diag_descr_snomed) | diag_descr_snomed %in% c("Referral to service")) |>
  mutate(diag_descr_snomed = if_else(diag01_code == "", "none", diag_descr_snomed))  |> 
  filter(is.na(diag_descr_snomed)) 

df_consistency_preplot |> saveRDS(here("data", "251204_qdf_diag_consistency.rds"))

df_consistency_preplot |>
  ggplot(aes(as_date(year_quarter), p, group = diag_descr_snomed, col = diag_descr_snomed))+
  geom_line(alpha = .3)+
  geom_smooth(method = "lm", se = F)+
  geom_point(size = 0.6)+
  geom_blank(aes(y = 0))+
  facet_wrap(vars(der_provider_site_code))+
  # facet_wrap(vars(der_provider_site_code), scales = "free_y")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  theme(
    strip.text = element_text(size = 5.5),
    legend.position = "none",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
  )


# 2022/23 - QUARTER 2
# 2023/24 - QUARTER 2 
# 2024/25 - QUARTER 2
# 2025/26 - QUARTER 2  (3 YEARS)

# DO THEM BOTH - SO EVERYTHING - SENSITIVITY ANALYSIS?

# MATCH WHAT YOU CAN

# BUT WE'VE ALREADY FILTERED AND ACCEPTED SOME LEVEL OF VARIATION IN NA

# SO PERHAPS, BASED ON THESE GRAPHICS, FILTER A FEW MORE AND 
# MATCH THE DIAGNOSES THAT YOU CAN THAT ARE INCONSISTENT
# PROBABLY THE TOP 25 EACH QUARTER WILL COVER THOSE

# REFERAL TO SERVICE AND SUCH BECOME NA - MAY HAVE TO GO BACK AND 
# RECALC SAMPLE

df_sample_consistent <- df_consistency_preplot |> 
  filter(is.na(diag01_code)) |> 
  group_by(der_provider_site_code) |> 
  nest() |> 
  mutate(model = map(data, \(df) lm(p ~ year_quarter, data = df))) |> 
  mutate(gradient = map_dbl(model, function(obj){
    tidy(obj) |> 
      filter(term == "year_quarter") |> 
      pull(estimate)
  }
    )) |> 
  arrange(desc(abs(gradient))) |> 
# EXCLUSIONS (based on average gradient > 0.008 % per day )
filter(gradient < 0.00008) |> 
# EXCLUSION BASED ON HIGH MISSINGS FOR EXTENDED PERIOD:
filter(der_provider_site_code != "RTF86") |> 
select(der_provider_site_code) 

# df_sample_consistent |> saveRDS(here("data", "251204_df_sample_consistent.rds"))

# RA701
# RXPCP
# RXF10

# EXCLUSION (based on high variability short term -  25% )
# RTF86

# WE'D BE LEFT WITH 16 PROVIDERS.

# 365*0.0000925*6.5


