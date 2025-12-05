

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_representation <- here("sql", "[UDAL]phase_II_query_representation.sql")

query_diag_representation <- readChar(
  sql_script_representation, 
  file.info(sql_script_representation)$size
  ) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")


df_representation <- dbGetQuery(con_test, query_diag_representation) |>
  as_tibble() |>
  clean_names() 

gc()
gc()
gc()
gc()

# -------------------------------------------------------------------------

eric_2425 <- read_csv(
  "https://files.digital.nhs.uk/9A/DF80E7/ERIC%20-%202024_25%20-%20Trust%20data.csv",
  col_select = 1:3
  ) |> 
  clean_names() |> 
  mutate(der_provider_code = str_sub(trust_code, 1, 3)) |> 
  rename(region = commissioning_region)

# -------------------------------------------------------------------------

# df_representation |> colnames()

df_representation_t1 <- df_representation |> 
  filter(is_type3 == 0) |> 
  left_join(eric_2425, join_by(der_provider_code)) 


# df_representation |> 
#   left_join(eric_2425, join_by(der_provider_code)) |> 
#   filter(is.na(commissioning_region)) |> 
#   count(der_provider_code) |> 
#   print(n=50)

# TODO NEED PROVIDER SURVIVAL OVER 7 YEARS TO Q1 

# df_providers_ever_present1 |> saveRDS("df_providers_ever_present1.rds")
# df_provider_sample_part_2of2 |> saveRDS("df_provider_sample_part_2of2.rds")
# df_provider_sample_part_1point5of2 |> saveRDS("df_provider_sample_part_1point5of2.rds")

df_providers_ever_present <- readRDS(here("data", "df_providers_ever_present1.rds"))
df_sample_consistent <- readRDS(here("data", "251204_df_sample_consistent.rds"))

gc()
gc()
gc()
gc()
gc()
gc()
gc()

# THEN YOU HAVE TWO DFs: POPULATION (SURVIVORS) AND SAMPLE

# gt table listing providers in sample 

# df_ecds_II <- arrow::open_dataset(here::here("data_raw", "251124_sample.gzip.parquet")) |>
#   collect()
# 
# df_ecds_II_sample <- df_ecds_II |> 
#   filter(der_ec_arrival_date_time < as_date("2025-07-01")) |>
#   filter(!der_provider_code %in% c("RJE")) |>  

df_sample_sites <- df_providers_ever_present |> 
  semi_join(df_sample_consistent) |> 
  mutate(id = row_number(), .before = der_provider_site_code) |> 
  select(id, site_name, trust_name) 
df_sample_sites |> saveRDS(here("data", "251204_df_sample_sites.rds"))

  gt() |>
  fmt_auto() |>
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(c(site_name, trust_name))
  ) |>
  tab_options(
    data_row.padding = px(1),
    table.font.size = "small"
  )


  
# table_providers_s2 <- providers_s2 |> 
#   left_join(eric_2223, join_by(procode == trust_code)) |> select(1,2) |> 
#   rename(Provider_Code = procode, Provider_Name = trust_name) |> 
#   arrange(Provider_Code) |> 
#   mutate(Provider_Name = str_to_title(Provider_Name)) |> 
#   mutate(Provider_Name = str_replace_all(Provider_Name, "Nhs", "NHS")) 


# -------------------------------------------------------------------------

table_representation <-
  tibble(
    data = list(
      df_representation_t1 |>
        semi_join(df_providers_ever_present, join_by(der_provider_site_code)),
      df_representation_t1 |>
        semi_join(df_sample_consistent, join_by(der_provider_site_code))
    )
  ) |>
  mutate(data_t3 = list(
    df_representation |>
      semi_join(df_providers_ever_present, join_by(der_provider_site_code)),
    df_representation |>
      semi_join(df_sample_consistent, join_by(der_provider_site_code))
      )) |> 
  mutate(n_sites = map_dbl(data, \(df)
  df |>
    count(der_provider_site_code) |>
    nrow())) |>
  mutate(size = map(data, \(df)
  df |>
    count(der_provider_site_code, wt = n) |>
    group_by(der_provider_site_code) |>
    # MEAN OF A PROVIDER'S SIZE OVER 1 YEARS:
    reframe(mean_n = mean(n)) |>
    mutate(size_group = case_when(
      mean_n < 65e3 ~ "attendances < 65k",
      mean_n >= 65e3 & mean_n < 85e3 ~ "attendances 65-84k",
      mean_n >= 85e3 & mean_n < 105e3 ~ "attendances 85-104k",
      mean_n >= 105e3 ~ "attendances >= 105k",
      T ~ NA_character_
    )) |>
    mutate(size_group = factor(
      size_group,
      levels = c("attendances < 65k", "attendances 65-84k", "attendances 85-104k", "attendances >= 105k")
    )) |>
    count(size_group) |>
    mutate(p = n / sum(n)) |>
    select(-n) |>
    pivot_wider(names_from = size_group, values_from = p))) |>
  # MEAN SITE ARRIVAL BY AMBULANCE:
  mutate(arrive_ambulance = map_dbl(data, \(df)
  df |>
    count(is_ambulance, wt = n) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    filter(is_ambulance == 1) |>
    summarise(av = round(mean(p), 3)) |>
    pull(av))) |>
  mutate(admitted = map_dbl(data, \(df)
  df |>
    count(is_adm, wt = n) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    filter(is_adm == 1) |>
    summarise(av = round(mean(p), 3)) |>
    pull(av))) |>
  mutate(age_under_18 = map_dbl(data, \(df)
  df |>
    count(age_under_18, wt = n) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    filter(age_under_18 == 1) |>
    summarise(av = round(mean(p), 3)) |>
    pull(av))) |>
  mutate(age_75_plus = map_dbl(data, \(df)
  df |>
    count(age_75_plus, wt = n) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    filter(age_75_plus == 1) |>
    summarise(av = round(mean(p), 3)) |>
    pull(av))) |>
  mutate(deprived_20pc = map_dbl(data, \(df)
  df |>
    count(imd_quint_1, wt = n) |>
    mutate(p = n / sum(n)) |>
    ungroup() |>
    filter(imd_quint_1 == 1) |>
    summarise(av = round(mean(p), 3)) |>
    pull(av))) |>
  mutate(four_hour = map(data, \(df)
                         df |> 
                           count(der_provider_site_code, under_4hrs, wt = n) |> 
                           group_by(der_provider_site_code) |> 
                           mutate(p = n/sum(n)) |> 
                           ungroup() |> 
                           filter(under_4hrs ==1) |> 
                           arrange(p) |> 
                           # mutate(grp = case_when(
                           #   p < 0.48 ~ "4h wait standard < 48%",
                           #   p >= 0.48 & p < 0.58 ~ "4h wait standard 48%-58%",
                           #   p > 0.58  ~ "4h wait standard >= 58%",
                           #   TRUE ~ NA_character_
                           # )) |> 
                           # mutate(grp = factor(
                           #   grp,
                           #   levels = c(
                           #     "4h wait standard < 40%",
                           #     "4h wait standard 40%-59%",
                           #     "4h wait standard >= 60%"
                           #   )
                           # )) |>
                           mutate(grp = case_when(
                             p < 0.5 ~ "4h target < 46%",
                             p >= 0.5 & p < .6 ~ "4h target 46%-57%",
                             p >= 0.6 ~ "4h target >= 58%",
                             TRUE ~ NA_character_
                           )) |>
                           filter(!is.na(grp)) |> 
                           mutate(grp = factor(
                             grp,
                             levels = c(
                               "4h target >= 58%",
                               "4h target 46%-57%",
                               "4h target < 46%"
                             )
                           )) |>
                           count(grp) |> 
                           mutate(p = n / sum(n)) |>
                           select(-n) |>
                           pivot_wider(names_from = grp, values_from = p)
  )) |>
  mutate(twelve_hour = map(data, \(df)
                           df |> 
                             count(der_provider_site_code, under_12hrs, wt = n) |> 
                             group_by(der_provider_site_code) |> 
                             mutate(p = n/sum(n)) |> 
                             ungroup() |> 
                             filter(under_12hrs == 0) |> 
                             arrange(p) |> 
                             mutate(grp = case_when(
                               p < 0.07  ~ "12h waits < 7%",
                               p <= 0.14 & p >= 0.07 ~ "12h waits 7-13%",
                               p >= 0.14 ~ "12h waits >= 14%",
                               TRUE ~ NA_character_
                             )) |>
                             filter(!is.na(grp)) |> 
                             mutate(grp = factor(
                               grp,
                               levels = c(
                                 "12h waits < 7%",
                                 "12h waits 7-13%",
                                 "12h waits >= 14%"
                               )
                             )) |>
                             count(grp) |> 
                             mutate(p = n / sum(n)) |> 
                             select(-n) |>
                             pivot_wider(names_from = grp, values_from = p)
  )) |> 
  mutate(also_offers_type_3 = map_dbl(data_t3, \(df)
                                      df |>
                                        count(der_provider_site_code, is_type3, wt = n) |>
                                        count(is_type3) |> 
                                        summarise(p = round(n[[2]]/n[[1]], 3)) |> 
                                        pull(p)
  )) |> 
  mutate(region = map(data, \(df)
                      df |>
                        count(region, wt = n, sort = T) |>
                        mutate(p = n / sum(n)) |>
                        group_by(region) |>
                        summarise(av = round(mean(p), 3)) |>
                        ungroup() |>
                        pivot_wider(names_from = region, values_from = av) |>
                        clean_names() |>
                        rename_with(~ paste0("region_", .x, recycle0 = TRUE)) |>
                        identity())
         ) 

# table_representation$size[[1]]
# table_representation$size[[2]]

table_representation$region[[2]] |> 
  pivot_longer(cols = everything())

table_representation$four_hour[[1]]
table_representation$four_hour[[2]]



table_representation_2of2 <-  table_representation |>
  unnest(region) |>
  unnest(size) |>
  unnest(four_hour) |>
  unnest(twelve_hour) |>
  select(-c(data, data_t3)) |>
  mutate(zz = if_else(n_sites > 50, "population", "sample"), .before = n_sites) |>
  pivot_longer(cols = !starts_with("zz"), names_to = "variable") |>
  pivot_wider(names_from = zz, values_from = value) |> 
  # table_representation_trends |> 
  select(variable, population, sample) |> 
  mutate(across(2:3, ~if_else(is.na(.), 0, .))) |>
  rowwise() |> 
  mutate(rep_diff = sample/population)

# Comparative Representation in sample

table_representation_2ofx |> 
  print(n=30)
  
# table_representation$twelve_hour[[1]]
# table_representation$twelve_hour[[2]]
# 
# table_representation |> 
#   select(also_offers_type_3)
# 
# 
# table_representation$data_t3[[2]] |> 
#   count(der_provider_site_code, is_type3, wt = n) |>
#   count(is_type3) |> 
#   summarise(p = round(n[[2]]/n[[1]], 3)) |> 
#   pull(p)
# 

table_representation_2of2 |>  saveRDS(here("data", "251204_table_representation.rds"))

table_representation_2ofx |> 
  gt() |> 
  opt_row_striping(row_striping = F) |> 
  fmt_auto() |> 
  cols_label(
    rep_diff = "Representation:<br>Sample vs Pop",
    .fn = md
  ) |> 
  tab_style(
    style = list(
      cell_text(transform = "capitalize", weight = "bold")
    ),
    # different location
    # locations = cells_column_labels(everything())
    locations = cells_column_labels(1:3)
  ) |> 
  fmt_percent(rows = c(2:24), decimals = 1) |>
  fmt_percent(columns = 4, decimals = 0) |>
  sub_values(
    rows = 1,  fn = function(x) x >= 0 & x < 1,
             replacement = "") |> 
  tab_row_group(
    label = "Patient demographics:",
    rows =  str_detect(variable, "age|depriv|region_")
  ) |> 
  tab_row_group(
    label = "Case-mix-related:",
    rows =  str_detect(variable, "arrive_|admitted")
  ) |> 
  tab_row_group(
    label = "Provider-related (23/24):",
    rows =  str_detect(variable, "att|also|4h|12h")
  ) |> 
  tab_row_group(
    label = "",
    rows =  str_detect(variable, "n_sites|23")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = c(1, 2, 3, 4))
  ) |> 
  gt_highlight_cols(sample, fill = "grey", alpha = 0.1) |> 
  tab_options(
    data_row.padding = px(1),
    table.font.size = "small"
  )
