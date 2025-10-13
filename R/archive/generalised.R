# README
# Run early workflow 1-3 and then:

source(here("R", "funs.R"))
source(here("R", "model_specifications.R"))
# source(here("R", "dq_thresholds.R"))
source(here("R", "provider_dq.R"))


# ________----

# param_provider <- "RBD" # (B)
# param_provider <- "RAX" # KINGSTON AND RICHMOND (E)
param_provider <- "RWE" # LEICS - NUMBERS ISSUE
param_provider <- "RMC" # BOLTON (D) ARG WORST DQ
# # param_provider <- "R1H" # BARTS (D)
param_provider <- "RJ7" # UH ST. GEORGE'S (C)
# param_provider <- "RYR" # SUSSEX (A)
# param_provider <- "RC9" # BEDFORD (A)


df_model_specs_assigned <- df_provider_dataqual |> 
  left_join(
    df_thresholds, 
    join_by(
      p_diag >= min_diag,
      p_acuity >= min_acuity,
      p_complnt >= min_complnt,
      p_arrmode >= min_arrmode,
      p_refsorc >= min_refsorc,
      )
    ) |> 
  group_by(der_provider_code) |> 
  filter(rank == min(rank)) |>
  ungroup() |> 
  select(-c(starts_with("min"), rank))

# df_model_specs_assigned |> count(mod_spec) |> mutate(p = n/sum(n)) 

df_model_specs_assigned |> 
  filter(mod_spec == "d") |> 
  # arrange(-p_diag)
  mutate(a = pmax(p_diag, p_complnt)) |> 
    arrange(-a) |> 
  print(n=60)
  
df_model_specs_assigned |> 
  filter(mod_spec == "e") 

df_chosen_provider_spec <- df_model_specs_assigned |> 
  filter(der_provider_code == param_provider) |> 
  left_join(lkp_model_specs, join_by(mod_spec)) 



# # A PROVIDER'S MIN DATA QUAL FOR ANY WEEK WITHIN STUDY PERIOD: 
# df_model_specs_prep <- calc_perc_na_weekly(der_ec_diagnosis_all) |> 
#   left_join(calc_perc_na_weekly(ec_acuity_snomed_ct), join_by(der_provider_code)) |> 
#   left_join(calc_perc_na_weekly(ec_chief_complaint_snomed_ct), join_by(der_provider_code)) |> 
#   left_join(calc_perc_na_weekly(ec_arrival_mode_snomed_ct), join_by(der_provider_code)) |> 
#   left_join(calc_perc_na_weekly(ec_attendance_source_snomed_ct), join_by(der_provider_code)) |> 
#   rename(
#     p_complnt = p_chief_complaint,
#     p_arrmode = p_arrival_mode,
#     p_refsorc =p_attendance_source
#   ) |> 
#   # ASSUMING SOME CORRELATION BETWEEN ARRMODE AND ACUITY:
#   mutate(mod_spec = case_when(
#     p_diag >= 0.85 & p_acuity >= 0.85 & p_arrmode >= 0.85 & p_refsorc >= 0.85 ~ "A", 
#     # (B) LOWER DIAG THRESHOLD, REMOVE ACUITY AND REFSORC CRITERIA:
#     p_diag >= 0.80 & p_arrmode >= 0.85 ~ "B",
#     # (C) USE COMPLAINT AND ACUITY:
#     p_complnt >= 0.80 & p_acuity >= 0.80 ~ "C",
#     # ARRIVAL MODE:
#     p_arrmode >= 0.80 ~ "D",
#     # BASE MODEL: DEMOGRAPHICS (AGE, SEX, ETH, IMD) AND TIME RELATED: 
#     TRUE ~ "E" 
#   )) 

# TODO FLOW DIAGRAM BASED ON DQ

# df_model_specs_prep |>  count(mod_spec) |> mutate(p = n/sum(n))
# df_model_specs_prep |> filter(mod_spec == "E") |> print(n=50)
# df_model_specs_prep |> filter(der_provider_code == "R1H") 

# df_model_specification |> 
#   mutate(
#     model_formula = case_when(
#       mod_spec == "a" ~ basic_e + spec_a,
#       mod_spec == "b" ~ basic_e + spec_b,
#       mod_spec == "c" ~ basic_e + spec_c,
#       mod_spec == "d" ~ basic_e + spec_d,
#       mod_spec == "e" ~ basic_e,
#       T ~ NA_character_
#       )
#   )

# vec_model_spec <- df_chosen_provider_spec |> pull(mod_formula)


# df_model_specs <- df_model_specs_prep |> 
#   select(der_provider_code, mod_spec) |> 
#   mutate(model_formula = case_when(
#     mod_spec == "A" ~ "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + diag01 + arrmode_ambulance + acuity + referral_source + wkend + night_time_8to8",
#     mod_spec == "B" ~ "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + diag01 + arrmode_ambulance + wkend + night_time_8to8",
#     mod_spec == "C" ~ "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + complaint + acuity + wkend + night_time_8to8",
#     mod_spec == "D" ~ "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + arrmode_ambulance + wkend + night_time_8to8",
#     mod_spec == "E" ~ "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + wkend + night_time_8to8",
#     T ~ NA_character_
#   ))

# vec_model_spec <- df_model_specs |> 
#   filter(der_provider_code == param_provider) |> 
#   pull(model_formula)


# 4. PREP 1----------------------------------------------------------------------

df_1 <- df_ecds_raw |> 
  filter(der_provider_code == param_provider) |>
  mutate(diag01_code = str_extract(der_ec_diagnosis_all, "^[^,]*")) |> 
  ### JOIN TO REFERENCE DF FOR TEXT DESRIPTION OF SNOMED CODES:
  left_join(df_ref_trimmed, join_by(ec_arrival_mode_snomed_ct == snomed_code)) |>
  rename(ref_arrmode = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_attendance_source_snomed_ct == snomed_code)) |>
  rename(ref_attsrc = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
  rename(ref_disdest = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_acuity_snomed_ct == snomed_code)) |>
  rename(ref_acuity = derived_snomed_descr)
  # left_join(df_ref_trimmed, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |>
  # rename(ref_chief_complaint = derived_snomed_descr) |>
  # left_join(df_ref_trimmed, join_by(accommodation_status_snomed_ct == snomed_code)) |>
  # rename(ref_accom_status = derived_snomed_descr) |>


gc()
gc()

df_1 |> 
  count(ec_chief_complaint_snomed_ct, sort = T) |> 
  left_join(df_ref, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |> 
  select(-n, everything(), n, -c(ecds_group3, ecds_group2)) |> 
  mutate(p = n/sum(n)) |> 
  mutate(cs = cumsum(p)) 

# ________----


## a. ENCODING PARTS 1 AND 2 -----------------------------------

if(
  df_chosen_provider_spec$mod_spec %in% c("a", "b")
) {
### i. diagnosis -----------------------------------------------

tmp_diag_prep_l3 <- df_1 |> 
  left_join(df_ref_diagnosis, join_by(diag01_code == snomed_code)) |> 
  count(diag01_code, diag_descr_snomed, ecds_group3, ecds_group2, ecds_group1, sort = T) |> 
  mutate(p = n/sum(n)) |> 
  mutate(cs = cumsum(p)) 

tmp_diag_prep_l2_l1 <- tmp_diag_prep_l3 |>
  slice(-c(1:30)) |>
  count(diag01_code, ecds_group2, ecds_group1, wt = p, sort = T, name = "p") |>
  group_by(ecds_group2) |> 
  mutate(g2 = sum(p)) |> 
  ungroup() |> 
  arrange(desc(g2)) |> 
  group_by(g = desc(g2)) |> 
  mutate(group_id = (cur_group_id())) |> 
  ungroup() 

# FOR MOST FREQ 30 SNOMED CODES (L3 = LEVEL 3 = HIGHEST DETAIL)
tmp_diag_l3 <- tmp_diag_prep_l3 |>
  slice(1:30) |>
  select(diag01_code, diag01 = diag_descr_snomed, n, p, cs) |>
  mutate(diag01 = janitor::make_clean_names(diag01)) |> 
  mutate(diag01 = str_c("l3_", diag01))

## THEN TOP 10 LEVEL 2 GIVES ~20% MORE COVERAGE: 
tmp_diag_l2 <- tmp_diag_prep_l2_l1 |>
  filter(group_id %in% 1:10) |> 
  select(diag01_code, diag01 = ecds_group2, p) |> 
  mutate(diag01 = janitor::make_clean_names(diag01, allow_dupes = T)) |> 
  mutate(diag01 = str_c("l2_", diag01))

# REMAINING ARE GROUPED AT LEVEL 1 (LEAST DETAIL) OR... AEA GROUPS
tmp_diag_l1 <- tmp_diag_prep_l2_l1 |> 
  filter(group_id >= 11) |> 
  group_by(ecds_group1) |> 
  mutate(g1 = sum(p)) |> 
  ungroup() |> 
  mutate(ecds_group1 = if_else(
    g1 < 0.01, "other_engineered", ecds_group1
  )) |> 
  select(diag01_code, diag01 = ecds_group1, p) |> 
  mutate(diag01 = janitor::make_clean_names(diag01, allow_dupes = T)) |> 
  mutate(diag01 = str_c("l1_", diag01))

# PROVIDER SPECIFIC:
lkp_diag <- bind_rows(
  tmp_diag_l3,
  tmp_diag_l2,
  tmp_diag_l1
) |>
  select(diag01_code, diag01)

df_encoding_part_1_of_3 <- df_1 |>
  left_join(lkp_diag, join_by(diag01_code)) |> 
  # SET REF CATEGORY FOR DIAGNOSIS: (HIGH VOLUME, MODERATE EFFECT)
  mutate(diag01 = fct_relevel(diag01, "l3_lower_respiratory_tract_infection")) 

} else if (
  df_chosen_provider_spec$mod_spec == "c"
) { 
### ii. chief complaint -----------------------------------------------------------
# options(scipen = 999)


# TODO IF WE WANTED TO IMPROVE MODEL, THERE ARE CERTAIN COMPLAINTS
# WITH A VERY HIGH CHANCE OF ADMISSION. MAKE SURE THESE ARE UNGROUPED.
# ALSO APPLICABLE TO DIAGNOSIS CODES.

# df_encoding_part_3_of_4 |> 
#   count(ref_chief_complaint, ed_discharged, sort = T) |> 
#   group_by(ref_chief_complaint) |> 
#   mutate(n_total = sum(n)) |> 
#   mutate(p = n/sum(n)) |> 
#   ungroup() |> 
#   mutate(rank = dense_rank(desc(n_total))) |>
#   complete(ref_chief_complaint, ed_discharged) |> 
#   rename(p_adm = p) |> 
#   filter(ed_discharged == 1) |> 
#   arrange(-p_adm) |> 
#   mutate(p = n_total/sum(n_total, na.rm = T)) |> 
#   # mutate(csp = cumsum(p)) |>
#   print(n=30)

# tmp_diag_l3 <- tmp_diag_prep_l3 |>
#   slice(1:30) |>
#   select(diag01_code, diag01 = diag_descr_snomed, n, p, cs) |>
#   mutate(diag01 = janitor::make_clean_names(diag01)) |> 
#   mutate(diag01 = str_c("l3_", diag01))

tmp_complaint_prep <- df_1 |> 
  count(ec_chief_complaint_snomed_ct, sort = T) |> 
  left_join(df_ref, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |> 
  select(-n, everything(), n, -c(ecds_group3, ecds_group2)) |> 
  mutate(p = n/sum(n)) |> 
  mutate(cs = cumsum(p)) 

tmp_complaint_l2 <- tmp_complaint_prep |> 
  slice(1:50) |> 
  select(ec_chief_complaint_snomed_ct, complaint = derived_snomed_descr ) |>
  mutate(complaint = janitor::make_clean_names(complaint)) |> 
  mutate(complaint = str_c("l2_", complaint))

tmp_complaint_l1 <-tmp_complaint_prep |> 
  slice(-c(1:50)) |> 
  select(ec_chief_complaint_snomed_ct, complaint = ecds_group1 ) |>
  mutate(complaint = janitor::make_clean_names(complaint, allow_dupes = T)) |> 
  mutate(complaint = str_c("l1_", complaint))

# NOT PROVIDER SPECIFIC:
lkp_complaint <- bind_rows(
  tmp_complaint_l2,
  tmp_complaint_l1
) 

df_encoding_part_1_of_3 <- df_1 |>
  left_join(lkp_complaint, join_by(ec_chief_complaint_snomed_ct)) |> 
  # SET REF CATEGORY FOR COMPLAINT: (HIGH VOLUME, MODERATE EFFECT)
  mutate(diag01 = fct_relevel(complaint, "Chest pain")) 


} else {
  
  df_encoding_part_1_of_3 <- df_1
  
} 

gc()
gc()

### iii. other variables -----------------------------------------------------------

df_encoding_part_2_of_3 <- df_encoding_part_1_of_3 |>
  mutate(acuity = str_remove_all(ref_acuity, " level emergency care")) |>
  mutate(acuity = if_else(is.na(acuity), "NA", acuity)) |>
  mutate(ed_discharged = if_else(
    ref_disdest %in% c(
      "Discharge to ward",
      "Emergency department discharge to coronary care unit",
      "Emergency department discharge to high dependency unit",
      "Emergency department discharge to intensive care unit",
      "Emergency department discharge to operating theatre"
    ),
    0, 1
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
  # QUESTION ONLY EMERGENGY AMBULANCES ??
  mutate(arrmode_ambulance = case_when(
    ref_arrmode %in% c("Arrival by emergency road ambulance", "Arrival by non-emergency road ambulance", "Arrival by emergency road ambulance with medical escort") ~ "1",
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
  mutate(year_week = yearweek(der_ec_arrival_date_time)) |>
  mutate(wkend = if_else(
    wday(der_ec_arrival_date_time, week_start = 1) %in% 6:7,
    1, 0
  )) |>
  mutate(night_time_8to8 = if_else(
    hour(der_ec_arrival_date_time) %in% 8:19,
    0, 1
  )) 

# df_encoding_part_3_of_4 |> 
#   mutate(yearweek("2025-01-06"))
#   count(year_week, wkend)

# as_date(yearweek("2025-01-08"))


# ________----

# 5. DATA QUAL 1-------

## a. where clause exclusion details ----------------------------------

param_provider_site <- "RXK02"
param_provider_site <- "RBL20"

# CHECK FOR LOW ATTENDANCES - POSSIBLE DEAD CODE ERROR
param_provider_site <- "RWEAA"# LEICS
param_provider_site <- "RAL27"# MIDLESX
param_provider_site <- "RA7C2" # 
param_provider_site <- "N6J7V" # PRINCESS ROYAL
param_provider_site <- "R0A03" # 
param_provider_site <- "RF4DG" # 

# ALL DISCHARGED HOME:

param_provider_site <- "RBS25" # ALDERHEY - LOTS SUS EXCLUSIONS BASED ON STREAMING
param_provider_site <- "RF4DG" # KING GEORGE

# RUN WHERE CLAUSE SQL SCRIPT AT SAME TIME AS MAIN QUERY UPDATED.
# readRDS(here("data", "where_exclusion_matrix.rds")) |> 
# readRDS(here("data", "where_exclusions.rds")) |> 
# readRDS(here("data", "where.rds")) |> 
readRDS(here("data", "where_matrix.rds")) |> 
  filter(der_provider_site_code == param_provider_site) |> 
  mutate(sigma = sum(n)) |>
  rowwise() |> 
  mutate(row_sum = sum(c_across(c(everything(), -c(der_provider_code,der_provider_site_code, n, p, sigma))))) |> 
  ungroup() |> 
  # REMOVE THE ROW CORRESPONDING TO ATTENDANCES FOLLOWING ALL EXCLUSIONS:
  filter(row_sum != 0) |> 
  mutate(n_after_excl = sigma - cumsum(n)) |> 
  # view()
  mutate(n_before_excl = n_after_excl + n) |> 
  select(-c(sigma, p)) |> 
  rename(n_excl = n) |> 
  relocate(c(n_before_excl, n_excl, n_after_excl), .after = row_sum) |> 
  # print(n=40)
  view("where")



## b. date-related ---------------------------------------------------------

# TIMESERIES OF ATTENDANCES FOR LAST FIVE MONTHS TO AID SELECTION OF CUT-OFF DATE:
df_encoding_part_2_of_3 |> 
  count(date_dep = date(der_ec_departure_date_time)) |> 
  # FROM FIVE MONTHS PRIOR TO TODAY:
  filter(
    date_dep >= as_date(Sys.time()) - months(5) &
      date_dep <= as_date(Sys.time())
    ) |> 
  mutate(wkday = as.factor(wday(date_dep, label = T, week_start = 1))) |> 
  ggplot() +
  geom_point(aes(date_dep, n, col = wkday)) +
  geom_line(aes(date_dep, n), linewidth = 0.4)+
  # geom_smooth(aes(date_dep, n), se = F) +
  scale_color_brewer()+
  theme_minimal() +
  geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "2 weeks",date_labels = "%d %b")+
  NULL

# TIMESERIES OF % ADMITTED FOR LAST FIVE MONTHS TO AID SELECTION OF CUT-OFF DATE:
df_encoding_part_2_of_3 |> 
  count(date_dep = date(der_ec_departure_date_time), ed_discharged) |> 
  group_by(date_dep) |> 
  mutate(p = n/sum(n)) |> 
  ungroup() |> 
  complete(date_dep, ed_discharged) |> 
  filter(ed_discharged == 0) |> 
  # FROM FIVE MONTHS PRIOR TO TODAY:
  filter(
    date_dep >= as_date(Sys.time()) - months(5) &
      date_dep <= as_date(Sys.time())
    ) |> 
  mutate(wkday = as.factor(wday(date_dep, label = T, week_start = 1))) |> 
  ggplot() +
  geom_point(aes(date_dep, p, col = wkday)) +
  geom_line(aes(date_dep, p), linewidth = 0.4)+
  # geom_smooth(aes(date_dep, n), se = F) +
  scale_color_brewer()+
  theme_minimal() +
  geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "2 weeks",date_labels = "%d %b")+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Date", y = "% Patients admitted")+
  NULL

# ________----


# 6. ENCODING PART 3 --------------------------------------------------------

# TODO THEN CHOOSE:
# BASED ON ASSESSMENT OF ABOVE DQ:
# OFFER ONLY SUNDAY DATES (WEEK ENDING) TO CHOOSE FROM
# AS IT HAPPENS LAST DAY OF AUG WAS SUNDAY.
param_date_cutoff <- "2025-08-31"
# WE MIGHT WANT A DEFAULT 

# as_date(yearweek(param_date_cutoff)) - days(1)


# (REQUIRED BEFORE MOVING TO FURTHER DQ CHECKS)

df_encoding_part_3_of_3 <- df_encoding_part_2_of_3 |> 
    ##### DATE RELATED SELECTIONS:
    filter(year_week <= yearweek(param_date_cutoff)) |> 
    # WE START FROM FIRST MONDAY OF THE YEAR.
    # THE 7TH JAN WILL ALWAYS BE IN THE WEEK WE WANT TO START WITH.
    filter(year_week >= yearweek("2025-01-07")) |> 
    #####
    ## EXCLUDING DIAG AND COMPLAINT AS REF LEVEL ALREADY SPECIFIED:
    mutate(across(c(everything(), - age, -starts_with("diag")), ~ as.factor(.))) |>
    # EXPLICITLY SET REF LEVEL FOR OUTCOME (NECESSARY FOR CONSISTENT METRICS):
    mutate(ed_discharged = fct_relevel(ed_discharged, "0")) |> 
    # SET REF LEVEL FOR OTHER CATS: (HIGH VOLUME, MODERATE EFFECT)
    mutate(acuity = fct_relevel(acuity, "Standard")) |> 
    mutate(ethnic_grp_sus = fct_relevel(ethnic_grp_sus, "British, Mixed British")) |> 
    mutate(referral_source = fct_relevel(referral_source, "self")) |> 
    # SET REF CATEGORY FOR YEAR_MONTH (SECOND WEEK OF CALENDAR YEAR):
    mutate(year_week = fct_relevel(year_week, as.character(yearweek("2025-01-08")))) |>
    identity()


# df_encoding_part_3_of_3 |> glimpse()
# # levels(df_encoding_part_4_of_4$year_week)
# # levels(df_model_prep$diag01)
# levels(df_encoding_part_3_of_3$acuity)
levels(df_encoding_part_3_of_3$ed_discharged)

# df_encoding_part_3_of_3 |> 
#   count(acuity, arrmode_ambulance) |> 
#   group_by(acuity) |> 
#   mutate(p = n/sum(n)) |> 
#   ungroup() |> 
#   filter(arrmode_ambulance == 1)
  
# ________----

# 7. DATA QUAL 2-------

## c. regressor dq ----------------------------------------------------
# 
# # TODO CONTINGENT ON SELECTION OF DATE
# # TODO AS ABOVE - SEPTEMBER REMOVED AS UNFINISHED.
# 
# # NOTE: THE DIFFERENCE HERE IS THAT THIS IS OVERALL COMPLETENESS
# #       AND NOT MINIMUM COMPLETENESS IN A WEEK:
# 
# df_encoding_part_3_of_3 |> 
#   select(c(
#     ethnic_grp_sus,
#     imd_quint,
#     starts_with("diag01$"),
#     starts_with("complaint"),
#     ec_acuity_snomed_ct,
#     ec_arrival_mode_snomed_ct,
#     ec_attendance_source_snomed_ct
#   )) |>
#   names() |>
#   map(
#     ~ count(df_encoding_part_3_of_3, .data[[.x]], sort = T) |>
#       mutate(p = n / sum(n))
#   ) |> 
#   map(list(.%>% mutate(var = (names(.))[1]))) |> 
#   map(list(.%>% rename(level = 1))) |> 
#   map(list(.%>% mutate(level = as.character(level)))) |> 
#   reduce(bind_rows) |> 
#   select(var, everything()) |> 
#   count(var, na = is.na(level), wt = p, name = "p") |> 
#   complete(var, na) |> 
#   mutate(p = if_else(is.na(p), 0, p)) |> 
#   filter(na == FALSE) |> 
#   select(-na) |> 
#   # rename(%_complete = p)
#   # mutate(p = str_c(round(p *100, 2), "%")) |>
#   rename(prop_complete = p) 
# 

# TODO: DIAGNOSIS / COMPLAINT DIAGNOSTICS: (?)

# DIAGNOSIS CODING: DIAGNOSTICS
# (could write tests here)

tmp_diag_prep_l3 |> filter(diag_descr_snomed == "No abnormality detected") |> select(diag_descr_snomed, p)
tmp_diag_prep_l3 |> filter(is.na(diag_descr_snomed))

# tmp_diag_l3 |> summarise(perc_coverage_l3 = max(cs))
# tmp_diag_l2 |> summarise(perc_coverage_l2 = sum(p))
# tmp_diag_l1 |> summarise(perc_coverage_l1 = sum(p))

lkp_diag |> count(str_sub(diag01, 1, 2), diag01) |> nrow() == 52

lkp_diag

df_encoding_part_3_of_3 |> count(diag01, sort = T)

tmp_diag_l3

# ________----

# 7. MODEL  -----------------------------------------------------------

mod <- mgcv::gam(
  formula = as.formula(df_chosen_provider_spec$mod_formula),
  family = "binomial",
  data = df_encoding_part_3_of_3
) 

# TODO WE'D HAVE TO ADDRESS SOME NAs THIS WAY
# TODO WE'D HAVE TO ADDRESS SOME NAs THIS WAY
# TODO WE'D HAVE TO ADDRESS SOME NAs THIS WAY
# TODO WE'D HAVE TO ADDRESS SOME NAs THIS WAY
# TODO WE'D HAVE TO ADDRESS SOME NAs THIS WAY
mod$fitted.values |> length()
df_1

# mod <- mgcv::gam(
#   formula = ed_discharged ~ 
#     # # VAR OF INTEREST:
#     year_week +    
#     # CASE-MIX (DEMOGRAPHICS):
#     s(age, by = sex) + sex + imd_quint + ethnic_grp_sus +
#     # CASE-MIX (MEDICAL):
#     diag01 + arrmode_ambulance + acuity + referral_source +
#     # TIME-RELATED:
#     wkend + night_time_8to8,
#   family = "binomial",
#   # method = "REML",
#   data = df_encoding_part_4_of_4 
# ) |> 
#   # assign(str_c("mod_", param_provider), value = _)
#   identity()

# mod <-  get(str_c("mod_", param_provider))

mod |>
  saveRDS(
    here("data", str_c(
      "tmp_mod_wk_spec_", df_chosen_provider_spec |> pull(mod_spec),
      "_", param_provider,
      ".rds"
    ))
  )

mod <- readRDS(
  here("data", str_c(
    "tmp_mod_wk_spec_", df_provider_mod_spec |> pull(mod_spec),
    "_", param_provider,
    ".rds"
  ))
)
# ________----

# 8. RESULTS --------------------------------------------------------------

mod |> broom::glance()


tibble(
  obs = df_encoding_part_3_of_3$ed_discharged,

fit = fitted(mod), # predicted probabilities 1
predict(mod, type = "response" )
) |> 
  print(n=40)
# BRIER AND ROC C STAT.  


tibble(
  obs = df_encoding_part_3_of_3$ed_discharged,
  
  # predict(mod, type = "response") |>  tibble()
  fit = fitted(mod), # predicted probabilities 1
  
  # res = residuals(mod)
) |> 
  sample_n(1000) |> 
  ggplot(aes(obs, fit))+
  ggbeeswarm::geom_quasirandom(alpha = 0.1)+
  geom_boxplot(alpha = .2)

df_encoding_part_3_of_3 |> count(der_provider_code)

levels(df_encoding_part_3_of_3$ed_discharged)

tibble(
  obs = df_encoding_part_3_of_3$ed_discharged,
  
  # predict(mod, type = "response") |>  tibble()
  fit = fitted(mod), # predicted probabilities 1
  
  # res = residuals(mod)
) |> 
  # roc_auc(obs, fit, event_level = "second")
  brier_class(truth = obs, fit)


  roc_auc(
    tibble(
      obs = df_encoding_part_3_of_3$ed_discharged |>  length(),
      
      # predict(mod, type = "response") |>  tibble()
      fit = fitted(mod) |> length(), # predicted probabilities 1
      
      # res = residuals(mod)
    ),
    
    obs, fit)
  brier_class(obs, fit)

df_encoding_part_3_of_3 |> 
  summarise(across(everything(), ~ in))

tibble(
obs = df_encoding_part_3_of_3$ed_discharged,

# predict(mod, type = "response") |>  tibble()
fit = fitted(mod), # predicted probabilities 1

# res = residuals(mod)
) |>  
  filter(fit < 0.5)
mod$residuals |> tibble()


df_1 |> vctrs::vec_size()
df_encoding_part_2_of_3 |> vctrs::vec_size()
df_encoding_part_3_of_3 |> vctrs::vec_size()

df_encoding_part_3_of_3 |> 
  select(
    ed_discharged, age, sex, imd_quint, ethnic_grp_sus,
    year_week, wkend, night_time_8to8, 
    diag01, arrmode_ambulance, acuity, referral_source
    
    ) |> 
  mutate(across(everything(), ~is.na(.))) |> 
  colSums()
  
  
  # na.omit()
  colSums(is.na(.))

predict(mod) |>  tibble()
mod$residuals |> tibble()


two_class_example |> tibble() 
  roc_auc(two_class_example, truth, Class1)
  brier_class(two_class_example, truth, Class1)
  # roc_auc(two_class_example, obs, prob class 1)


mod$
mod$residuals |> tibble()

  install.packages("yardstick")
  library("yardstick")

yardstick::roc_auc()

# roc_auc(two_class_example, truth, Class1)
two_class_example |> tibble()
# truth | Class 1  |Class 2(1-Class1) | predicted (fit)
# 0
# 1

results <- mod |> 
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

results |> 
  select(term, ..odds__ = odds, ..lci__ = lci, ..uci__ = uci, significance) |>
  print(n=150)



# ________----

# 9. PLOT --------------------------------------------------------------

# lkp_month <- month(1:12, label = T) |> 
#   enframe() |> 
#   mutate(yr =2025) |> 
#   mutate(term = str_c("year_month", yr, " ", value)) |> 
#   mutate(date = make_date(yr, name, 1)) |> 
#   select(term, date)

start_date <- get_first_monday(2025)


# (WEEK BEGINNING)
lkp_week <- seq(
  start_date,
  as_date(Sys.time()),
  by = "1 week"
  ) |> 
  enframe(name = NULL, value = "date") |> 
  mutate(term = yearweek(date)) |> 
  mutate(term = str_c("year_week", term))
  

# NEW ---------------------------------------------------------------------

prep_xmr <- results |> 
  select(term, estimate) |> 
  filter(str_detect(term, "year_week")) |>
  left_join(lkp_week, join_by(term)) |> 
  add_row(estimate = 0, date = start_date) |> 
  arrange(date) |> 
  ###   
  mutate(mr = abs(estimate - lag(estimate))) |> 
  mutate(x_mean = mean(estimate)) |> 
  mutate(mr_mean = mean(mr, na.rm = T)) |> 
  mutate(ucl = x_mean + (2.66*mr_mean)) |> 
  mutate(lcl = x_mean - (2.66*mr_mean)) |> 
  mutate(ucl_mr = mr_mean*3.268) |> 
  mutate(across(where(is.numeric), ~ exp(.))) |> 
  identity()
  
# X CHART
plot_x <- prep_xmr |> 
  ggplot(aes(date, estimate))+
  geom_hline(aes(yintercept = x_mean[1]), lty = "dashed")+
  geom_hline(aes(yintercept = ucl[1]), lty = "dashed")+
  geom_hline(aes(yintercept = lcl[1]), lty = "dashed")+
  annotate("text", x = start_date , y = 1.1*max(prep_xmr$estimate), hjust = 0, label = "↑ Higher admission threshold")+
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_y_log10()+
  theme(axis.title = element_text(size = 7))+
  scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
  labs(
    x = "\nDate",
    y = str_c(
      "Casemix-adjusted odds of ED discharge (",
      param_provider, ")\n(Reference category: w/c Jan 6th 2025)\n"
    )
  )

  # MR CHART  
plot_mr <- prep_xmr |> 
  ggplot(aes(date, mr))+
  geom_hline(aes(yintercept = mr_mean[1]), lty = "dashed")+
  geom_hline(aes(yintercept = ucl_mr[1]), lty = "dashed")+
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_y_log10()+
  theme(axis.title = element_text(size = 7))+
  scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
  labs(
    x = "\nDate",
    y = str_c(
      "Moving range (",
      param_provider, ")\n"
    )
  )
  
plot_x / plot_mr


# # TODO: INCLUDE REFERENCE CAT IN MEDIAN OR NOT????
# # RUN CHART:
# results |> 
#   select(term, odds, lci, uci) |>
#   filter(str_detect(term, "year_month")) |>
#   left_join(lkp_month, join_by(term)) |> 
#   add_row(odds = 1, date = as_date("2025-05-01")) |>
#   mutate(med_odds = median(odds)) |> 
#   ggplot(aes(date, odds))+
#   geom_hline(aes(yintercept = med_odds), lty = "dashed")+
#   # geom_pointrange(aes(ymin = lci, ymax = uci), colour = "grey80") +
#   geom_point()+
#   geom_line()+
#   theme_minimal()+
#   scale_y_log10()+
#   theme(axis.title = element_text(size = 9))+
#   scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
#   labs(
#     x = "\nDate",
#     y = str_c(
#     "Casemix-adjusted odds of admission (",
#     param_provider, ")\n(Reference category: May 2025)\n"
#     )
#   )

# TODO: NOTE WE EXPECT INVERSE RELATIONSHIP BETWEEN ODDS OF ADMISSION
# AND ADMISSION THRESHOLDS:
# HIGHER ODDS = LOWER THRESHOLD
# THIS GRAPHIC MAY THEREFORE INCREASE COGNITIVE LOAD


# # TIMESERIES WITH CONFINT:
# results |>
#   select(term, odds, lci, uci) |>
#   filter(str_detect(term, "year_month")) |>
#   left_join(lkp_month, join_by(term)) |>
#   add_row(odds = 1, date = as_date("2025-05-01")) |>
#   mutate(med_odds = median(odds)) |>
#   ggplot(aes(date, odds))+
#   geom_hline(yintercept = 1, lty = "dashed")+
#   geom_pointrange(aes(ymin = lci, ymax = uci), colour = "grey50", size = 0.4) +
#   geom_point()+
#   # geom_line()+
#   theme_minimal()+
#   scale_y_log10()+
#   theme(axis.title = element_text(size = 9))+
#   scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
#   labs(
#     x = "\nDate",
#     y = "Casemix-adjusted odds of admission\n"
#     )
#     
#   
