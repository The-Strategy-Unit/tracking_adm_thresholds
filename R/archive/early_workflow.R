# README
# Test the "use R to query sql" workflow in UDAL.
# Using faster SUS plus.

library("DBI")
library("odbc")
library("here")
library("broom")
library("dplyr")
library("purrr")
library("tidyr")
# library("mgcv")
library("tibble")
library("forcats")
library("ggplot2")
library("ggrepel")
library("janitor")
library("stringr")
library("tsibble")
library("lubridate")
library("patchwork")
library("yardstick")

options(scipen=999) 

server <- "udalsyndataprod.sql.azuresynapse.net"
db <- "UDAL_Warehouse"

# server <- ""
# db <- ""

con_test <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = server,
  Database = db,
  Authentication = "ActiveDirectoryInteractive"
)
# REMEMBER POP-UP!


# 1. GET ECDS DATA -------------------------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_ecds <- here("sql", "[UDAL]query_ecds.sql")

query_ecds <- readChar(sql_script_ecds, file.info(sql_script_ecds)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_ecds_raw <- dbGetQuery(con_test, query_ecds) |>
  as_tibble() |>
  clean_names()

gc()
gc()
gc()

# TODO FIND PROVIDER LOOKUP !!!
df_ecds_raw |> count(der_record_type, der_financial_year)
# df_ecds_raw |> slice_sample(n = 50) |> view("ex_main")


## i. timeseries ---------------------------------------------------------
# 
# # READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
# sql_script_ecds_ts <- here("sql", "[UDAL]query_ecds_timeseries.sql")
# 
# query_ecds_ts <- readChar(sql_script_ecds_ts, file.info(sql_script_ecds_ts)$size) |>
#   str_replace_all(string = _, "\n|\r|ï»¿", " ")
# 
# df_ecds_ts_raw <- dbGetQuery(con_test, query_ecds_ts) |>
#   as_tibble() |>
#   clean_names()
# 
# gc()
# gc()
# gc()
# 
# # df_ecds_ts_raw |> arrange(arrival_date) |>  slice(1:50) |> view("test_ts")

## a. counts chr -------------------------------------------------------------

# tmp_counts_prep <-
#   df_ecds_raw |>
#   mutate(across(c(arrival_month), ~ as.character(.))) |>
#   select(where(is.character)) |>
#   # select(-c(
#   #   contains("lsoa"),
#   #   year_of_birth
#   # )
#   #   ) |>
#   # select(contains("postcode")) |>
#   slice_sample(n = 1e6) |>
#   # glimpse()
# 
#   # tmp_counts_prep |>
#   names() |>
#   map(~ count(df_ecds_raw, .data[[.x]], sort = T) |> mutate(p = n / sum(n))) |>
#   # map(~count(tmp_counts_prep, .data[[.x]], sort = T))|>
#   identity()


## a. counts num -------------------------------------------------------------

# df_ecds_raw |>
#   mutate(across(c(arrival_month), ~ as.character(.))) |>
#   select(where(is.numeric)) |>
#   select(deleted) |>
#   slice_sample(n = 1e6) |>
#   names() |>
#   map(~ count(df_ecds_raw, .data[[.x]], sort = T) |> mutate(p = n / sum(n)))



# 2. GET REFERENCE DATA FOR SNOMED CODES -------------------------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_reference <- here("sql", "[UDAL]query_ecds_reference.sql")

query_reference <- readChar(sql_script_reference, file.info(sql_script_reference)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_reference_raw <- dbGetQuery(con_test, query_reference) |>
  as_tibble() |>
  clean_names()

gc()
gc()
gc()


df_ref <- df_reference_raw |>
  mutate(sheet_name = str_to_lower(sheet_name)) |>
  # DESIRED FIELDS ONLY:
  filter(
    str_detect(
      sheet_name,
      "mode|acuity|accom|spoken|attend|discharge status|dest|complaint"
    )
  ) |>
  separate(col = sheet_name, into = c("sheet_no", "var"), sep = "^\\S*\\K\\s+") |>
  arrange(desc(created_date)) |>
  group_by(var, snomed_code) |>
  # TAKE MOST RECENT LOOKUPS:
  filter(row_number() == 1) |>
  ungroup() |>
  select(-c(sheet_no, created_date)) |>
  select(-var) 


df_ref_trimmed <- df_ref |> 
  select(snomed_code, derived_snomed_descr)

df_ref_trimmed |> 
  filter(snomed_code == "305398007")


df_ref_diagnosis <- df_reference_raw |>
  mutate(sheet_name = str_to_lower(sheet_name)) |>
  filter(str_detect(sheet_name, "diagnosis$")) |> 
  arrange(desc(created_date)) |>
  group_by(snomed_code) |>
  # TAKE MOST RECENT LOOKUPS:
  filter(row_number() == 1) |>
  ungroup() |>
  select(-created_date) |>
  rename(diag_descr_snomed = derived_snomed_descr)

# ___________----


# 3. GET OTHER REFERENCE TABLES -------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_type1s <- here("sql", "[UDAL]query_dept_t1_ref.sql")

query_type1s <- readChar(sql_script_type1s, file.info(sql_script_type1s)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

lkp_type1s_raw <- dbGetQuery(con_test, query_type1s) |>
  as_tibble() |>
  clean_names() 
  

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_ethref <- here("sql", "[UDAL]query_eth_ref.sql")

query_ethref <- readChar(sql_script_ethref, file.info(sql_script_ethref)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

lkp_ethref_raw <- dbGetQuery(con_test, query_ethref) |>
  as_tibble() |>
  clean_names() |> 
  select(-udal_file_id) |> 
  rename(ethnic_grp_sus = description)

gc()
gc()
gc()


# ___________----

# xi. DQ OV.ALL ---------------------------------------------------------------

# df_ecds |>
# slice_sample(n= 100) |>
# transmute(a = as_date(der_ec_arrival_date_time))


## a. daily attds ------------------------------------------
# AT ATTENDANCE LEVEL, 1 MONTH LAG APPEARS SUFFICIENT

df_ecds_ts_raw |>
  mutate(date = as_date(arrival_date)) |>
  arrange(date) |>
  mutate(year = as.character(year(date))) |>
  mutate(date = make_date(year = 2025, month = month(date), day = day(date))) |>
  filter(date >= "2025-06-01" & date <= "2025-08-31") |>
  ggplot() +
  # geom_point(aes(date, n, col = year))+
  # geom_line(aes(date, n, col = year), linewidth = 0.4)+
  geom_smooth(aes(date, n, col = year)) +
  theme_minimal() +
  scale_x_date(date_breaks = "month")

## b. var % ------------------------------------------

# 26% NA
df_ecds_raw |>
  count(der_ec_diagnosis_all, sort = T) |>
  mutate(p = n / sum(n))

# 20% NA
df_ecds_raw |>
  count(ec_chief_complaint_snomed_ct, sort = T) |>
  mutate(p = n / sum(n))

# 6% NA
df_ecds_raw |>
  count(ec_acuity_snomed_ct, sort = T) |>
  mutate(p = n / sum(n))

# WHAT'S IT LIKE OVER TIME?

## c. daily var ----------------------------------------------------------

### i.  diag ----------------------------------------------------------

# PROBABLY 2 MONTHS FOR DIAG
testing_testing <- df_ecds_raw |>
  slice_sample(n = 1e3) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, has_diag = !is.na(der_ec_diagnosis_all))
###

testing_testing |>
  tidyr::complete(date, has_diag) |>
  filter(is.na(n))

arrange(has_diag) |>
  group_by(date) |>
  mutate(p_diag = n[2] / sum(n)) |>
  ungroup() |>
  filter(has_diag == TRUE) |>
  ggplot() +
  geom_point(aes(date, p_diag)) +
  geom_line(aes(date, p_diag), linewidth = 0.4) +
  geom_smooth(aes(date, p_diag), se = F) +
  theme_minimal() +
  # geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "month")

### ii.  complaint ----------------------------------------------------------
# STEP CHANGE AT TURN OF FYEAR
df_ecds_raw |>
  slice_sample(n = 1e6) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, has_complaint = !is.na(ec_chief_complaint_snomed_ct)) |>
  arrange(has_complaint) |>
  group_by(date) |>
  mutate(p_complaint = n[2] / sum(n)) |>
  ungroup() |>
  filter(has_complaint == TRUE) |>
  ggplot() +
  geom_point(aes(date, p_complaint)) +
  geom_line(aes(date, p_complaint), linewidth = 0.4) +
  geom_smooth(aes(date, p_complaint), se = F) +
  theme_minimal() +
  geom_blank(aes(y = 0)) +
  scale_x_date(date_breaks = "month")


### iii. acuity ----------------------------------------------------------

df_ecds_raw |>
  slice_sample(n = 1e6) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, has_acuity = !is.na(ec_acuity_snomed_ct)) |>
  arrange(has_acuity) |>
  group_by(date) |>
  mutate(p_acuity = n[2] / sum(n)) |>
  ungroup() |>
  filter(has_acuity == TRUE) |>
  ggplot() +
  geom_point(aes(date, p_acuity)) +
  geom_line(aes(date, p_acuity), linewidth = 0.4) +
  geom_smooth(aes(date, p_acuity), se = F) +
  theme_minimal() +
  geom_blank(aes(y = 0)) +
  scale_x_date(date_breaks = "month")

# ___________----

# xii. DQ PROV ---------------------------------------------------------------------

# TODO GENERALISE THIS TO INCLUDE ALL REQUIRED VARIABLES.
# SEE SECTION 12 (ASSUMING NO MORE EXCLUSIONS - TRUE)


### i.  diag ----------------------------------------------------------

df_prov_p_diag <- df_ecds_raw |>
  # slice_sample(n = 1e6) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, der_provider_code, has_diag = !is.na(der_ec_diagnosis_all)) |>
  complete(date, der_provider_code, has_diag) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(has_diag) |>
  group_by(date, der_provider_code) |>
  mutate(p_diag = n / sum(n)) |>
  ungroup() |>
  # arrange(date) |>
  filter(has_diag == TRUE) |>
  ###
  # filter(der_provider_code == "RRK")
  group_by(der_provider_code) |>
  mutate(p_mean = mean(p_diag, na.rm = T)) |>
  ungroup() |>
  count(der_provider_code, p_mean) |>
  arrange(desc(p_mean))
# print(n =121)
# ggplot()+
# geom_point(aes(date, p_diag))+
# geom_line(aes(date, p_diag), linewidth = 0.4)+
# geom_smooth(aes(date, p_diag), se = F)+
# theme_minimal()+
# # geom_blank(aes(y=0))+
# scale_x_date(date_breaks = "month")


### ii.  complaint ----------------------------------------------------------

df_prov_p_complaint <- df_ecds_raw |>
  # slice_sample(n = 1e6) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, der_provider_code, has_complaint = !is.na(ec_chief_complaint_snomed_ct)) |>
  complete(date, der_provider_code, has_complaint) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(has_complaint) |>
  group_by(date, der_provider_code) |>
  mutate(p_complaint = n / sum(n)) |>
  ungroup() |>
  # arrange(date) |>
  filter(has_complaint == TRUE) |>
  ###
  # filter(der_provider_code == "RRK")
  group_by(der_provider_code) |>
  mutate(p_mean = mean(p_complaint, na.rm = T)) |>
  ungroup() |>
  count(der_provider_code, p_mean) |>
  arrange(desc(p_mean))

### iii.  acuity ----------------------------------------------------------

df_prov_p_acuity <- df_ecds_raw |>
  # slice_sample(n = 1e6) |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, der_provider_code, has_acuity = !is.na(ec_acuity_snomed_ct)) |>
  complete(date, der_provider_code, has_acuity) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(has_acuity) |>
  group_by(date, der_provider_code) |>
  mutate(p_acuity = n / sum(n)) |>
  ungroup() |>
  # arrange(date) |>
  filter(has_acuity == TRUE) |>
  ###
  # filter(der_provider_code == "RRK")
  group_by(der_provider_code) |>
  mutate(p_mean = mean(p_acuity, na.rm = T)) |>
  ungroup() |>
  count(der_provider_code, p_mean) |>
  arrange(desc(p_mean))


### iv. selection --------------------------------------------------------

df_prov_p_diag |>
  # IGNORE COMPLAINT:
  # left_join(df_prov_p_complaint, join_by(der_provider_code)) |>
  left_join(df_prov_p_acuity, join_by(der_provider_code)) |>
  # select(-c(n, n.x, n.y)) |>
  select(-c(n.x, n.y)) |>
  # rename(
  #   mn_diag = p_mean.x,
  #   mn_comp = p_mean.y,
  #   mn_acui = p_mean
  # ) |>
  rename(
    mn_diag = p_mean.x,
    mn_acui = p_mean.y
  ) |>
  rowwise(der_provider_code) |>
  mutate(mean = mean(c_across(starts_with("mn_")))) |>
  ungroup() |>
  arrange(desc(mean)) |> 
  mutate(rank = row_number()) |> 
  filter(der_provider_code == "RC9")

df_ecds_raw |>
  # filter(der_provider_code %in% c("RLT", "RCX", "RXC", "REM", "RWA")) |>
  # filter(der_provider_code %in% c("RYR", "RAJ", "RNZ")) |>
  filter(der_provider_code %in% c("RYR", "RAJ", "RNZ", "RBK")) |>
  count(der_provider_code)

# TODO SELECTION MAY CHANGE WERE WE TO INCLUDE INPATIENT DATA 
# (TIMES FOR BED OCCUPANCY)

# ___________----

# RYR - UH SUSSEX 
# xiii. DQ BEDFORD (RC9)  ------------------------------------------------------

### i. diag -----------------------------------------------------------

df_ecds_raw |>
  filter(der_provider_code == "RC9") |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, has_diag = !is.na(der_ec_diagnosis_all)) |>
  complete(date, has_diag) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(has_diag) |>
  group_by(date) |>
  mutate(p_diag = n / sum(n)) |>
  ungroup() |>
  # arrange(date) |>
  filter(has_diag == TRUE) |>
  ggplot() +
  geom_point(aes(date, p_diag)) +
  geom_line(aes(date, p_diag), linewidth = 0.4) +
  geom_smooth(aes(date, p_diag), se = F) +
  theme_minimal() +
  # geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "month")+
  scale_y_continuous(labels = scales::percent)

### ii. acuity -----------------------------------------------------------

df_ecds_raw |>
  filter(der_provider_code == "RC9") |>
  mutate(date = as_date(der_ec_arrival_date_time)) |>
  count(date, has_acuity = !is.na(ec_acuity_snomed_ct)) |>
  complete(date, has_acuity) |>
  mutate(n = if_else(is.na(n), 0, n)) |>
  arrange(has_acuity) |>
  group_by(date) |>
  mutate(p_acuity = n / sum(n)) |>
  ungroup() |>
  # arrange(date) |>
  filter(has_acuity == TRUE) |>
  ggplot() +
  geom_point(aes(date, p_acuity)) +
  geom_line(aes(date, p_acuity), linewidth = 0.4) +
  geom_smooth(aes(date, p_acuity), se = F) +
  theme_minimal() +
  # geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "month")+
  scale_y_continuous(labels = scales::percent)

# ___________----

# 4. BEGIN RC9 ----------------------------------------------------------------------

df_rc9 <- df_ecds_raw  |> 
  filter(der_provider_code == "RC9") |>
  # filter(der_provider_code == "RYR")
  # filter(der_provider_code == "RBK")
  # filter(der_provider_code == "RRK")
  mutate(diag01_code = str_extract(der_ec_diagnosis_all, "^[^,]*")) |> 
  ### JOIN TO REFERENCE FOR TEXT DESRIPTION OF SNOMED:
  left_join(df_ref_trimmed, join_by(accommodation_status_snomed_ct == snomed_code)) |>
  rename(ref_accom_status = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_arrival_mode_snomed_ct == snomed_code)) |>
  rename(ref_arrmode = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_attendance_source_snomed_ct == snomed_code)) |>
  rename(ref_attsrc = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
  rename(ref_disdest = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |>
  rename(ref_chief_complaint = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_acuity_snomed_ct == snomed_code)) |>
  rename(ref_acuity = derived_snomed_descr)
  # left_join(df_ref_trimmed, join_by(preferred_spoken_language_snomed_ct == snomed_code)) |>
  # rename(language = derived_snomed_descr) |>
  # left_join(df_ref_trimmed, join_by(ec_discharge_status_snomed_ct == snomed_code)) |>
  # rename(disstat = derived_snomed_descr) |>

gc()
gc()
gc()

# df_rc9 |> count()
# df_rc9 |> colnames()

# df_rc9 |>
#   select(where(is.character)) |>
#   # select(der_age_at_cds_activity_date, der_number_ec_diagnosis) |>
#   names() |>
#   map(~ count(df_ryr, .data[[.x]], sort = T) |> mutate(p = n / sum(n)))


# # df_rc9 |> count(is.na(attendance_lsoa_treatment_site_distance))
# df_rc9 |>
# # df_ecds_raw |> 
# #   filter(der_provider_code == "RYR") |> 
#   mutate(distance = as.numeric(attendance_lsoa_treatment_site_distance)) |> 
#   summarise(
#     med = median(distance, na.rm = T),
#     mean = mean(distance, na.rm = T)
#   )


### x. eda correlation (?): diagnosis completeness and coding of nothing abnormal ------

# df_cor_a <- df_ecds_raw |>
#   count(
#     der_provider_code,
#     has_diag = !is.na(der_ec_diagnosis_all),
#   )|>
#   complete(has_diag) |>
#   mutate(n = if_else(is.na(n), 0, n)) |>
#   group_by(der_provider_code) |>
#   mutate(p_diag = n / sum(n)) |>
#   ungroup() |>
#   filter(has_diag == TRUE)
# 
# 
# df_cor_b <- df_ecds_raw |>
#   count(
#     der_provider_code,
#     diag_no_abnml = der_ec_diagnosis_all == "281900007"
#   )|>
#   complete(diag_no_abnml) |>
#   mutate(n = if_else(is.na(n), 0, n)) |>
#   group_by(der_provider_code) |>
#   mutate(p_diag_no_abnml = n / sum(n)) |>
#   ungroup() |>
#   filter(diag_no_abnml == TRUE)
# 
# df_cor_c <- df_ecds_raw |>
#   # mutate(date = as_date(der_ec_arrival_date_time)) |>
#   count(
#     der_provider_code,
#     has_acuity = !is.na(ec_acuity_snomed_ct)
#   )|>
#   complete(has_acuity) |>
#   mutate(n = if_else(is.na(n), 0, n)) |>
#   group_by(der_provider_code) |>
#   mutate(p_acuity = n / sum(n)) |>
#   ungroup() |>
#   filter(has_acuity == TRUE)


# df_cor_a |>
#   left_join(df_cor_b, join_by(der_provider_code)) |>
#   left_join(df_cor_c, join_by(der_provider_code)) |>
#   mutate(x = mean(p_diag_no_abnml, na.rm = T)) |>
#   filter(p_diag > 0.98) |>
#   ggplot() +
#   geom_point(aes(p_diag, p_diag_no_abnml, size = n.x, colour = p_acuity)) +
#   geom_text_repel(aes(p_diag, p_diag_no_abnml, label = der_provider_code), size = 2)+
#   # geom_line(aes(date, p_diag), linewidth = 0.4) +
#   geom_hline(aes(yintercept = x), lty = "dotted")+
#   geom_smooth(aes(p_diag, p_diag_no_abnml), method = "lm", se = F) +
#   theme_minimal() +
#   geom_blank(aes(y=0))+
#   scale_color_viridis_c(end = 0.96, labels = scales::percent)+
#   # scale_x_date(date_breaks = "month")+
#   scale_y_continuous(labels = scales::percent)+
#   scale_x_continuous(labels = scales::percent)

# % ACUITY COMPLETE FOR SOME OF THESE IS LOW



# ___________----


# 5. DIAG ENCODING -----------------------------------------------
# TODO NOTE: APPEARS THAT TRUSTS USE DIFFERENT SET OF SNOMED CODES
# E.G. SUSSEX USE CODES NOT COVERED BY LOOKUP - IT'S BIG (3K) PROBLEM
# TODO NOTE: VARIABLE STYLES OF CODING E.G. NO ABNORMALITIES

# FOR RYR SUSSEX.
# df_reference_raw |> 
#   count(sheet_name)
#   # filter(str_detect(snomed_code, "25374005"))
#   filter(str_detect(derived_snomed_descr, "gastroent")) |> 
#   print(n=50)
# "25374005"

df_ref_diagnosis <- df_reference_raw |>
  mutate(sheet_name = str_to_lower(sheet_name)) |>
  filter(str_detect(sheet_name, "diagnosis$")) |> 
  # separate(col = sheet_name, into = c("sheet_no", "var"), sep = "^\\S*\\K\\s+") |>
  arrange(desc(created_date)) |>
  group_by(snomed_code) |>
  # TAKE MOST RECENT LOOKUPS:
  filter(row_number() == 1) |>
  ungroup() |>
  select(-created_date) |>
  rename(diag_descr_snomed = derived_snomed_descr) 
  # select(-c(sheet_no, created_date, var)) |>

# ODD:
# FOR SUSSEX:
# (NO ABNORMALITIES  AMOUNT TO 30% OF ALL ATTENDANCES)
# 33% OF NO ABNORMALITIES ARE ADMITTED 
# df_ecds_raw |>
#   filter(der_provider_code == "RYR") |>
#   left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
#   rename(ref_disdest = derived_snomed_descr) |>
#   mutate(admitted = if_else(
#     ref_disdest %in% c(
#       "Discharge to ward",
#       "Emergency department discharge to coronary care unit",
#       "Emergency department discharge to high dependency unit",
#       "Emergency department discharge to intensive care unit",
#       "Emergency department discharge to operating theatre"
#     ),
#     1, 0
#   )) |>
#   count(der_ec_diagnosis_all == "281900007", admitted) |>
#   mutate(p = n/sum(n))

# FOR BEDFORD:
# (NO ABNORMALITIES  AMOUNT TO 13% OF ALL ATTENDANCES)
#  22% OF NO ABNORMALITIES ADMITTED :

# df_rc9_plus |>
#   count(der_ec_diagnosis_all == "281900007", admitted) |>
#   mutate(p = n/sum(n))

# SO RC9 LOOKS FINE. 
# IT APPEARS SOME TRUSTS USE DIFFERENT SET OF SNOMED CODES
tmp_rc9_diags_prep <- df_rc9 |> 
  left_join(df_ref_diagnosis, join_by(diag01_code == snomed_code)) |> 
  count(diag01_code, diag_descr_snomed, ecds_group3, ecds_group2, ecds_group1, sort = T) |> 
  mutate(p = n/sum(n)) |> 
  mutate(cs = cumsum(p)) |> 
  select(-n) |>
  # filter(is.na(diag_descr_snomed)) |>
  # filter(is.na(diag01_code)) |>
  identity()

# FOR MOST FREQ 30 SNOMED CODES (L3 = LEVEL 3 = HIGHEST DETAIL)
tmp_rc9_diags_l3 <- tmp_rc9_diags_prep |>
  slice(1:30) |>
  select(diag01_code, diag01 = diag_descr_snomed) |> 
  mutate(diag01 = janitor::make_clean_names(diag01)) |> 
  mutate(diag01 = str_c("l3_", diag01))
  

# 10 DIAGNOSES GIVE ~30% COVERAGE
# 30 DIAGNOSES GIVE ~46% COVERAGE
# 50 DIAGNOSES GIVE ~56% COVERAGE
# 100 DIAGNOSES GIVE ~72% COVERAGE


# lkp_rc9_diags_1 |> 
#   slice(-c(1:30)) |> 
#   count(ecds_group2, wt = p, sort = T, name = "p") |> 
#   mutate(cs = cumsum(p))  

tmp_rc9_diags_prep_2 <- tmp_rc9_diags_prep |>
  slice(-c(1:30)) |>
  count(diag01_code, ecds_group2, ecds_group1, wt = p, sort = T, name = "p") |>
  group_by(ecds_group2) |> 
  mutate(g2 = sum(p)) |> 
  ungroup() |> 
  arrange(desc(g2)) |> 
  group_by(g = desc(g2)) |> 
  mutate(group_id = (cur_group_id())) |> 
  ungroup() 

## THEN TOP 10 LEVEL 2 GIVES ~20% MORE COVERAGE: 
tmp_rc9_diags_l2 <- tmp_rc9_diags_prep_2 |>
  filter(group_id %in% 1:10) |> 
  select(diag01_code, diag01 = ecds_group2) |> 
  mutate(diag01 = janitor::make_clean_names(diag01, allow_dupes = T)) |> 
  mutate(diag01 = str_c("l2_", diag01))

# lkp_rc9_diags |> 
#   select(-c(n)) |> 
#   slice(-c(1:30)) |> 
#   count(ecds_group2, ecds_group1, wt = p, sort = T, name = "p") |> 
#   group_by(ecds_group2) |> 
#   mutate(sum_g2 = sum(p)) |> 
#   ungroup() |> 
#   arrange(desc(sum_g2)) |> 
#   group_by(g = desc(sum_g2)) |> 
#   mutate(group_id = (cur_group_id())) |> 
#   ungroup() |> 
#   # print(n=40) 
#   select(-g) 

# REMAINING ARE GROUPED AT LEVEL 1 (LEAST DETAIL) OR... AEA GROUPS
tmp_rc9_diags_l1 <- tmp_rc9_diags_prep_2 |> 
  filter(group_id >= 11) |> 
  group_by(ecds_group1) |> 
  mutate(g1 = sum(p)) |> 
  ungroup() |> 
  # 
  #count(ecds_group1, g1) |> 
  #arrange(-g1)
  #
  mutate(ecds_group1 = if_else(
    g1 < 0.01, "other_engineered", ecds_group1
  )) |> 
  select(diag01_code, diag01 = ecds_group1) |> 
  mutate(diag01 = janitor::make_clean_names(diag01, allow_dupes = T)) |> 
  mutate(diag01 = str_c("l1_", diag01))

# GENERALISED (NOT PROVIDER SPECIFIC)
lkp_rc9_diags <- bind_rows(
  tmp_rc9_diags_l3,
  tmp_rc9_diags_l2,
  tmp_rc9_diags_l1
)

# lkp_diags |> 
#   # count(str_sub(diag01, 1, 2))
#   count(str_sub(diag01, 1, 2), diag01) |> 
#   print(n=60)
# # THEN 11 GROUP 1 PLUS "OTHER" = 12 - SO ALL DIAGS IN 52 GROUPS.

df_rc9_plus_diag <- df_rc9 |>
  left_join(lkp_rc9_diags, join_by(diag01_code))
  

# ___________----

# 6. ENCODING OTHER VARIABLES -----------------------------------------------------------

df_model_prep_rc9 <- df_rc9_plus_diag |>
  mutate(ref_acuity = str_remove_all(ref_acuity, " level emergency care")) |>
  mutate(ref_acuity = if_else(is.na(ref_acuity), "NA", ref_acuity)) |>
  mutate(admitted = if_else(
    ref_disdest %in% c(
      "Discharge to ward",
      "Emergency department discharge to coronary care unit",
      "Emergency department discharge to high dependency unit",
      "Emergency department discharge to intensive care unit",
      "Emergency department discharge to operating theatre"
    ),
    1, 0
  )) |>
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
  # TODO ONLY EMERGENGY AMBULANCES ??
  mutate(arrmode_ambulance = case_when(
    ref_arrmode %in% c("Arrival by emergency road ambulance", "Arrival by non-emergency road ambulance", "Arrival by emergency road ambulance with medical escort") ~ "1",
    is.na(ref_arrmode) ~ "0",
    # ref_arrmode == "Arrival by public transport" ~ "public_trans",
    TRUE ~ "0"
  )) |> 

# df_model_prep <- df_model_prep |>
  left_join(lkp_ethref_raw, join_by(ethnic_category == code)) |>
  mutate(imd_quint = as.character(
    round_half_up(
      as.numeric(index_of_multiple_deprivation_decile) / 2
    )
  )) |>
  mutate(imd_quint = if_else(is.na(imd_quint), "NA", imd_quint)) |>
  rename(age = der_age_at_cds_activity_date) |>
  mutate(year_month = yearmonth(der_ec_arrival_date_time)) |>
  mutate(wkend = if_else(
    wday(der_ec_arrival_date_time, week_start = 1) %in% 6:7,
    1, 0
  )) |>
  mutate(night_time_8to8 = if_else(
    hour(der_ec_arrival_date_time) %in% 8:19,
    0, 1
  )) |>
  # TODO NOTE THAT SEPTEMBER REMOVED AS UNFINISHED:
  # TODO NOTE THAT SEPTEMBER REMOVED AS UNFINISHED:
  # TODO NOTE THAT SEPTEMBER REMOVED AS UNFINISHED:
  filter(as_date(year_month) < as_date("2025-09-01")) |> 
  mutate(across(c(everything(), -age), ~ as.factor(.))) |>
  # SET REF CATEGORY FOR DIAGNOSIS and OTHER CATS: (HIGH VOLUME, MODERATE EFFECT)
  mutate(diag01 = fct_relevel(diag01, "l3_lower_respiratory_tract_infection")) |> 
  mutate(ref_acuity = fct_relevel(ref_acuity, "Standard")) |> 
  mutate(ethnic_grp_sus = fct_relevel(ethnic_grp_sus, "British, Mixed British")) |> 
  mutate(referral_source = fct_relevel(referral_source, "self")) |> 
  # SET REF CATEGORY FOR YEAR_MONTH:
  mutate(year_month = fct_relevel(year_month, "2025 May")) |> 
  # mutate(across(c(year_month), ~ as.ordered(.))) |> 
  identity()

levels(df_model_prep$year_month)
levels(df_model_prep$diag01)
# df_model_prep |> glimpse()
  
# ___________----

## x. walkouts? ------

# TODO LOOK AT PATIENT WALKED OUT
df_model_prep_rc9 |>
  filter(diag01 == "l3_patient_walked_out") |>
  select(where(is.factor)) |>
  names() |>
  map(~ count(
    df_model_prep |>
      filter(diag01 == "l3_patient_walked_out"),
    .data[[.x]],
    sort = T
  ) |>
    mutate(p = n / sum(n)))
# filter(diag01 == "l3_lower_respiratory_tract_infection") |>
# count(ref_acuity, diag01, admitted, sort = T) |>
# group_by(ref_acuity) |>
# mutate(p = n/sum(n)) |>
# filter(admitted == 1)

# ___________----

# 7. DQ PROV -----------------------------------------------------

df_model_prep_rc9 |> 
  select(c(
    ethnic_grp_sus,
    imd_quint,
    diag01,
    ec_acuity_snomed_ct,
    ec_arrival_mode_snomed_ct,
    ec_attendance_source_snomed_ct
    # HAVE TO DO SOMETHING ELSE FOR YEAR MONTH: 
    # (ATTENDANCE LEVELS - HOW TO DECIDE WHEN LOW /CUTOFF POINT? )
    # year_month
    )) |>
  names() |>
  map(
    ~ count(df_model_prep, .data[[.x]], sort = T) |>
      mutate(p = n / sum(n))
    ) |> 
  map(list(.%>% mutate(var = (names(.))[1]))) |> 
  map(list(.%>% rename(level = 1))) |> 
  map(list(.%>% mutate(level = as.character(level)))) |> 
  reduce(bind_rows) |> 
  select(var, everything()) |> 
  count(var, na = is.na(level), wt = p, name = "p") |> 
  complete(var, na) |> 
  mutate(p = if_else(is.na(p), 0, p)) |> 
  filter(na == FALSE) |> 
  select(-na) |> 
  # rename(%_complete = p)
  # mutate(p = str_c(round(p *100, 2), "%")) |>
  rename(prop_complete = p) 



# ___________----

# 8. MODEL ---------------------------------------------------------------

df_model_prep_rc9_trimmed <- df_model_prep_rc9 |> 
  select(
    admitted,
    age, sex, ethnic_grp_sus, imd_quint,
    diag01, ref_acuity, arrmode_ambulance, referral_source, # ec_chief_complaint_snomed_ct, #!!
    year_month,
    wkend,
    night_time_8to8
  )

mod_0.1_rc9 <- mgcv::gam(
  formula = admitted ~ 
    # # VAR OF INTEREST:
    year_month +    
    # CASE-MIX (DEMOGRAPHICS):
    s(age, by = sex) + sex + imd_quint + ethnic_grp_sus +
    # CASE-MIX (MEDICAL):
    diag01 + arrmode_ambulance + ref_acuity + referral_source +
    # TIME-RELATED:
    wkend + night_time_8to8,
  family = "binomial",
  # method = "REML",
  data = df_model_prep_trimmed 
)

mod_0.1_rc9
summary(mod_0.1_rc9)

mod_0.1_rc9 |> saveRDS(here("data", "tmp_df_mod_0.1.rds"))
mod_0.1_rc9 <- readRDS(here("data", "tmp_df_mod_0.1.rds"))

mod_0.1_rc9 |> 
  broom::tidy(parametric = TRUE) |> 
  # filter(str_detect(term, "quint")) |>
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
  mutate(across(where(is.numeric), ~ round(., digits = 4))) |> 
  select(term, ..odds__ = odds, ..lci__ = lci, ..uci__ = uci, significance) |> 
  # filter(str_detect(term, "diag")) |> 
  # print(n=60)
  print(n=100)
  # saveRDS(here("data", "tmp_df_mod_0.1_results.rds"))
  identity()


# ___________----
# `-------------------------------------------------------------------------


# *APPENDIX*  ----
# # d. RXC - EAST SUSSEX ------------------------------------------------------
# 
# ### i. diag -----------------------------------------------------------
# 
# df_ecds_raw |>
#   filter(der_provider_code == "RXC") |>
#   mutate(date = as_date(der_ec_arrival_date_time)) |>
#   count(date, has_diag = !is.na(der_ec_diagnosis_all)) |>
#   complete(date, has_diag) |>
#   mutate(n = if_else(is.na(n), 0, n)) |>
#   arrange(has_diag) |>
#   group_by(date) |>
#   mutate(p_diag = n[2] / sum(n)) |>
#   ungroup() |>
#   # arrange(date) |>
#   filter(has_diag == TRUE) |>
#   ggplot() +
#   geom_point(aes(date, p_diag)) +
#   geom_line(aes(date, p_diag), linewidth = 0.4) +
#   geom_smooth(aes(date, p_diag), se = F) +
#   theme_minimal() +
#   # geom_blank(aes(y=0))+
#   scale_x_date(date_breaks = "month")
# 
# 
# ### ii. complaint -----------------------------------------------------------
# df_ecds_raw |>
#   filter(der_provider_code == "RXC") |>
#   mutate(date = as_date(der_ec_arrival_date_time)) |>
#   count(date, has_complaint = !is.na(ec_chief_complaint_snomed_ct)) |>
#   complete(date, has_complaint) |>
#   mutate(n = if_else(is.na(n), 0, n)) |>
#   arrange(has_complaint) |>
#   group_by(date) |>
#   mutate(p_complaint = n[2] / sum(n)) |>
#   ungroup() |>
#   # arrange(date) |>
#   filter(has_complaint == TRUE) |>
#   ggplot() +
#   geom_point(aes(date, p_complaint)) +
#   geom_line(aes(date, p_complaint), linewidth = 0.4) +
#   geom_smooth(aes(date, p_complaint), se = F) +
#   theme_minimal() +
#   # geom_blank(aes(y=0))+
#   scale_x_date(date_breaks = "month")
# 
# 
# # . TEST RXC ----------------------------------------------------------------------
# df_rxc_raw <- df_ecds_raw |>
#   filter(der_provider_code == "RXC")
# 
# df_ref_trimmed <- df_ref |> 
#   select(snomed_code, derived_snomed_descr)
# 
# df_rxc <- df_rxc_raw  |> 
#   left_join(df_ref_trimmed, join_by(accommodation_status_snomed_ct == snomed_code)) |>
#   rename(ref_accom_status = derived_snomed_descr) |>
#   # left_join(df_ref_trimmed, join_by(preferred_spoken_language_snomed_ct == snomed_code)) |>
#   # rename(language = derived_snomed_descr) |>
#   left_join(df_ref_trimmed, join_by(ec_arrival_mode_snomed_ct == snomed_code)) |>
#   rename(ref_arrmode = derived_snomed_descr) |>
#   left_join(df_ref_trimmed, join_by(ec_attendance_source_snomed_ct == snomed_code)) |>
#   rename(ref_attsrc = derived_snomed_descr) |>
#   # left_join(df_ref_trimmed, join_by(ec_discharge_status_snomed_ct == snomed_code)) |>
#   # rename(disstat = derived_snomed_descr) |>
#   left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
#   rename(ref_disdest = derived_snomed_descr) |>
#   left_join(df_ref_trimmed, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |>
#   rename(ref_chief_complaint = derived_snomed_descr) |>
#   left_join(df_ref_trimmed, join_by(ec_acuity_snomed_ct == snomed_code)) |>
#   rename(ref_acuity = derived_snomed_descr)
# 
# gc()
# gc()
# gc()
# 
# df_rxc |>
#   select(where(is.character)) |>
#   # select(der_age_at_cds_activity_date, der_number_ec_diagnosis) |>
#   names() |>
#   map(~ count(df_rxc, .data[[.x]], sort = T) |> mutate(p = n / sum(n)))
# 
# # TODO TREAT NAs IN:
# # ACUITY
# 
# df_rxc_plus <- df_rxc |> 
#   mutate(ref_acuity = str_remove_all(ref_acuity, " level emergency care")) |>
#   # # count(ref_acuity)
#   mutate(admitted = if_else(
#     ref_disdest %in% c(
#       "Discharge to ward",
#       "Emergency department discharge to coronary care unit",
#       "Emergency department discharge to high dependency unit",
#       "Emergency department discharge to intensive care unit",
#       "Emergency department discharge to operating theatre"
#     ),
#     1, 0
#   )) |>
#   mutate(refsorc = case_when(
#     ref_attsrc %in% c("Referred by self", "Self-referral to accident and emergency department") ~ "self",
#     ref_attsrc == "Referred by ambulance service" ~ "ambulance",
#     ref_attsrc == "Referred by NHS 111 service" ~ "111",
#     ref_attsrc == "Referred by member of Primary Health Care Team" ~ "primary_care",
#     is.na(ref_attsrc) ~ "NA",
#     TRUE ~ "other"
#   )) |>
#   # TODO ONLY EMERGENGY AMBULANCES ??
#   mutate(arrmode_ambulance = case_when(
#     ref_arrmode %in% c("Arrival by emergency road ambulance", "Arrival by non-emergency road ambulance", "Arrival by emergency road ambulance with medical escort") ~ "1",
#     # ref_arrmode == "Arrival by public transport" ~ "public_trans",
#     is.na(ref_arrmode) ~ "0",
#     TRUE ~ "0"
#   )) |>
#   mutate(accomm_recorded = case_when(
#     ref_accom_status == "Housed" ~ "housed",
#     ref_accom_status %in% c(
#       "Lives in nursing home",
#       "Lives in a residential home",
#       "Lives in warden controlled accommodation",
#       "Lives in hospital"
#       ) ~ "nurs/resid home",
#     T ~ "other"
#   )) 
#   count(accommodation, sort = T) |> 
#     mutate(p = n/sum(n)) |> 
#   print(n = 40)
#   
#     # count(admitted) |> 
# 
# mgcv::gam(
#   formula = admitted ~
#     # # VAR OF INTEREST:
#     month_arr +
#     # DEMOGRAPHICS:
#     s(age, by = sex) + sex + imd_dec + ethnicity +
#     # CASE-MIX-RELATED:
#     arr_mode + acuity + diagnosis + refer_sorc +
#     # TIME-RELATED:
#     is_winter + is_wkend + is_night,
#   family = "binomial",
#   method = "REML",
#   data = df
# )
# 
# 
# 
# # # a. looking at diagnosis -----------------------------------------------
# 
# df_reference_raw |> 
#   count(sheet_name)
#   # filter(str_detect(snomed_code, "25374005"))
#   filter(str_detect(derived_snomed_descr, "gastroent")) |> 
#   print(n=50)
#   
# "25374005"
# 
# df_ref_diagnosis <- df_reference_raw |>
#   mutate(sheet_name = str_to_lower(sheet_name)) |>
#   # DESIRED FIELDS ONLY:
#   filter(str_detect(sheet_name, "diag")) |> 
#   separate(col = sheet_name, into = c("sheet_no", "var"), sep = "^\\S*\\K\\s+") |>
#   arrange(desc(created_date)) |>
#   group_by(var, snomed_code) |>
#   # TAKE MOST RECENT LOOKUPS:
#   filter(row_number() == 1) |>
#   ungroup() |>
#   filter( var != "diagnosis qualifier") |> 
#   # count(var)
#   select(-c(sheet_no, created_date, var)) |>
#   rename(diag_descr_snomed = derived_snomed_descr) |> 
#   # select(-var) |>
#   # group_by(var) |>
#   # nest() |>
#   # ungroup()
#   identity()
# 
# df_rxc |> 
#   count(der_number_ec_diagnosis, sort = T) |> 
#   # count(der_ec_diagnosis_all, sort = T) |> 
#   # left_join()
#   mutate(p = n/sum(n)) |> 
#   mutate(cs = cumsum(p)) |> 
#   print(n=100)
# 
# # ODD:
# # df_rxc_plus |> 
# #   count(der_ec_diagnosis_all == "281900007", admitted)
# 
# df_rxc_plus |> 
#   mutate(diag01 = str_extract(der_ec_diagnosis_all, "^[^,]*")) |> 
#   # filter(str_length(diag01) > 17) |> 
#   # count(diag01)
#   left_join(df_ref_diagnosis, join_by(diag01 == snomed_code)) |> 
#   count(diag01, diag_descr_snomed, ecds_group3, ecds_group2, ecds_group1, sort = T) |> 
#   mutate(p = n/sum(n)) |> 
#   mutate(cs = cumsum(p)) |> 
#   filter(is.na(diag_descr_snomed))
#   
#   print(n=50)
#   
# 
# "10050004, 1873941000000104, 211251003" |> 
#   # str_extract("[0-9]{5}")
#   # str_extract("^.*(?=(\\,))")
#   str_extract("^[^,]*")
