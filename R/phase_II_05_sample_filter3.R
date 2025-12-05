# README
# Exclude sites if there are dramatic changes in the outcome variable
# (here approximated with crude admission rates) over time.

df_sample_consistent |> saveRDS(here("data", "251204_df_sample_consistent.rds"))

df_ecds_II_sample <- df_ecds_II |> 
  # filter(!der_provider_code %in% c("RJE")) |>  
  semi_join(df_sample_consistent, join_by(der_provider_site_code))  |> 
  # SAMPLE DIAGNOSIS CODING OKAY TO END Q1 2025/26
  # BUT COMPLAINT CODING NAS HIGH IN Q1 2025/26, SO:
  filter(der_ec_arrival_date_time < as_date("2025-04-01"))

gc()
gc()
gc()
gc()
gc()

# TIMESERIES OF OUTCOME FOR SAMPLE ----------------------------------------

df_ecds_II |> colnames()

# df_ecds_II |> 
#   left_join(df_ref_trimmed, join_by(ec_discharge_status_snomed_ct == snomed_code)) |>
#   rename(ref_disstat = derived_snomed_descr) |> 
#   count(ec_discharge_status_snomed_ct, ref_disstat, sort = T)



df_outcome_ts <- df_ecds_II_sample |> 
  count(der_provider_site_code, site_name, trust_name, year_quarter, admitted) |> 
  # count(der_provider_site_code, site_name, trust_name)
  group_by(der_provider_site_code, site_name, trust_name, year_quarter) |> 
  mutate(p_adm = n/sum(n)) |> 
  filter(as_date(year_quarter) < as_date("2025-07-01")) |> 
  ungroup() |> 
  # filter(der_provider_site_code == "RJE01") |> 
  # arrange(year_quarter)
  filter(admitted == 1) 

df_outcome_ts |> saveRDS(here("data", "251204_dfq_outcome_ts.rds"))

df_outcome_ts |> 
  ggplot(aes(as_date(year_quarter), p_adm))+
  geom_line()+
  geom_point(size=.5)+
  geom_blank(aes(y= 0))+
  theme_minimal()+
  theme(
    strip.text = element_text(size = 6),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 9)
    )+
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.2, 0.4))+
  # facet_wrap(vars(str_c(der_provider_site_code, "\n", site_name)))+
  facet_wrap(vars(der_provider_site_code))+
  labs(x = "Date (quarter)", y = "% patients admitted")

# MAYBE 16 PROVIDERS WITH CONSISTENCY 
# (REMOVING LAST 2 QUARTERS AND BEING LENIENT WITH R1F)
# 
# df_ecds_II |> 
#   filter(der_financial_year == "2020/21") |> 
#   filter(der_provider_site_code == "RJE01") |> 
#   count(ref_disdest)
# 
# 
# df_provider_sample_part_2of2 <- df_provider_sample_part_1of2 |> 
#   filter(trust_name != "University Hospitals of North Midlands") 
#   
# df_provider_sample_part_2of2 <- readRDS("df_provider_sample_part_2of2.rds")
# 
# df_ecds_II_sample <- df_ecds_II |> 
#   # filter(!der_provider_code %in% c("RJE")) |>  
#   semi_join(df_provider_sample_part_2of2, join_by(der_provider_site_code)) |> 
#   filter(der_ec_arrival_date_time < as_date("2025-07-01"))
# 
# gc()
# gc()
# gc()
# gc()
# gc()


#  |>
#   arrow::write_parquet(
#     sink = here("data_raw", "251124_sample.gzip.parquet"),
#     compression = "gzip"
#   )
# 
#  <- arrow::open_dataset(here::here("data_raw", "251124_sample.gzip.parquet")) |>
#   collect()
  