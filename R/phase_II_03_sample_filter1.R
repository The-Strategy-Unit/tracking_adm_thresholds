# df_ecds_II_raw |>
#   arrow::write_parquet(
#     sink = here("data_raw", "251126_raw_II.gzip.parquet"),
#     compression = "gzip"
#   )
# 
df_ecds_II_raw <- arrow::open_dataset(here::here("data_raw", "251126_raw_II.gzip.parquet")) |>
  collect() |>  
  mutate(year_quarter = yearquarter(der_ec_arrival_date_time, fiscal_start = 4))

gc()
gc()
gc()
gc()


gc()
gc()
gc()
gc()

# 2. COUNT OF PROVIDERS ---------------------------------------------------

# 183 SITES TOTAL IN DATASET
# df_ecds_II_raw |> count(der_provider_site_code) |> count()


# 3. NAs BY VARIABLE ------------------------------------------------------

tmp_count <-  df_ecds_II_raw %>%
  # colnames(df_ecds_II_raw)[c()] %>%
  colnames() %>%
  map(~ count(df_ecds_II_raw, .data[["der_financial_year"]], is.na(.data[[.x]]), sort = T)) %>%
  map(list(. %>% mutate(var = (names(.))[2]))) %>%
  map(list(. %>% rename(cat = 2))) %>%
  map(list(. %>% mutate(is_na = as.character(cat)))) %>%
  reduce(bind_rows) %>%
  select(var, everything(), -cat) %>%
  arrange(var, -n) %>%
  group_by(var, der_financial_year) |>
  mutate(p = round(n/sum(n), 4)) |>
  ungroup() |>
  # filter(is_na == TRUE) |>
  mutate(var = str_remove_all(var, "[:punct:]" )) |>
  mutate(var = str_remove_all(var, "isnadata" )) |>
  mutate(var = snakecase::to_snake_case(var)) |>
  relocate(n, .before = p) |>
  arrange(var, desc(p))

gc()

# tmp_count |> view("na_counts")
#   saveRDS("from_ncdr_ecds_only_240917_dq_isna.RDS")

# 4. QUALITY BY PROVIDER (STEP 1) --------------------------------------------------

dq_providers <- df_ecds_II_raw |> 
  count(
    der_provider_site_code,trust_name, site_name,
    der_financial_year, year_quarter,
    na_diag = is.na(der_ec_diagnosis_all),
    na_acuity = is.na(ec_acuity_snomed_ct),
    # na_complaint = is.na(ec_chief_complaint_snomed_ct)
    ) 

gc()
gc()
gc()
gc()
gc()

dq_providers_long <- dq_providers |> 
  # complete(nesting(der_provider_site_code, trust_name, site_name, der_financial_year, year_quarter), na_diag, na_acuity, na_complaint) |> 
  complete(nesting(der_provider_site_code, trust_name, site_name, year_quarter), na_diag, na_acuity) |> 
  group_by(year_quarter, der_provider_site_code) |> 
  mutate(p = round(n/sum(n, na.rm = T), 4)) |>
  ungroup() |> 
  filter(if_all(starts_with("na"), ~ . == F))
  # filter(if_all(c(na_diag, na_acuity), ~ . == F)) 
  
# THRESHOLD | N PROVIDERS
# 90%         2
# 85%         7
# 84%         11
# 83%         14 ---
# 81%         16
# 80%         19 --- <<<


# -------------------------------------------------------------------------

# AND 138 SITES PRESENT OVER 27 QUARTERS TO 25/26
df_providers_ever_present <- dq_providers_long |>
  count(der_provider_site_code, trust_name, site_name, sort = T) |>
  filter(n == 27)
# -------------------------------------------------------------------------


df_provider_sample_part_1of2 <- dq_providers_long |> 
  semi_join(df_providers_ever_present, join_by(der_provider_site_code)) |> 
  # # group_by(der_provider_site_code) |> 
  # group_by(der_provider_site_code, year_quarter) |> #  na_diag, na_acuity, na_complaint, year_quarter
  # mutate(p = sum(p)) |> 
  # mutate(min_p_w_complaint = max(p)) |> 
  # ungroup() |> 
  # filter(if_all(starts_with("na_"), ~ . == F)) |> 
  group_by(der_provider_site_code) |> 
  mutate(min_p = min(p)) |> 
  ungroup() |> 
  filter(min_p > 0.75) |>
  distinct(der_provider_site_code, trust_name, site_name) |> 
  mutate(trust_name = str_remove_all(trust_name, " NHS Trust| NHS Foundation Trust")) |> 
  mutate(trust_name = str_remove_all(trust_name, " National Health Service "))
  
df_provider_sample_part_1of2 |> 
  print(n=40)

# 4. QUALITY BY PROVIDER (V2 COMPLAINT) --------------------------------------------------
# OF THOSE IDENTIFIED IN THE ABOVE, WHAT IS THEIR CODING OF COMPLAINT LIKE

dq_providers_complaint <- df_ecds_II_raw |> 
  semi_join(df_provider_sample_part_1of2, join_by(der_provider_site_code)) |> 
  count(
    der_provider_site_code,trust_name, site_name,
    year_quarter,
    na_complaint = is.na(ec_chief_complaint_snomed_ct)
    ) 

gc()
gc()
gc()
gc()
gc()

dq_providers_long_complaint <- dq_providers_complaint |> 
  # complete(nesting(der_provider_site_code, trust_name, site_name, der_financial_year, year_quarter), na_diag, na_acuity, na_complaint) |> 
  complete(nesting(der_provider_site_code, trust_name, site_name, year_quarter), na_complaint) |> 
  group_by(year_quarter, der_provider_site_code) |> 
  mutate(p = round(n/sum(n, na.rm = T), 4)) |>
  ungroup() |> 
  filter(if_all(starts_with("na"), ~ . == F))

# 12 WHERE OVER 80%
# 20 WHERE MIN OF ALL OVER 75%

df_provider_sample_stage_1.5 <- dq_providers_long_complaint |> 
  group_by(der_provider_site_code, trust_name, site_name) |> 
  summarise(min_p = min(p)) |> 
  ungroup() |> 
  arrange(min_p) |> 
  # mutate(min_p = min(p)) |> 
  # ungroup() |> 
  filter(min_p > 0.75) |>
  # distinct(der_provider_site_code, trust_name, site_name) |> 
  mutate(trust_name = str_remove_all(trust_name, " NHS Trust| NHS Foundation Trust")) |> 
  mutate(trust_name = str_remove_all(trust_name, " National Health Service ")) 
  







# -------------------------------------------------------------------------

# dq_providers_long |> 
#   select(der_provider_site_code, year_quarter, p) |> 
#   pivot_wider(names_from = year_quarter, values_from = p) |> 
#   identity()


# 5. EFFECT OF COVID ------------------------------------------------------

# ts_daily_ecds_only <- ecds_only |> 
#   count(fyear, date(dttm_arr))
