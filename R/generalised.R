# README
# Run early workflow 1-3 and then:

# i. rough provider dq ----------------------------------------------------

calc_perc_na <- function(x) {
  df_ecds_raw |> 
    count(der_provider_code, valid = !is.na({{x}})) |> 
    group_by(der_provider_code) |> 
    mutate("p_{{x}}" := n/sum(n)) |> 
    ungroup() |> 
    filter(valid == TRUE) |> 
    rename_with( 
      ~str_remove_all(. , "ec_|_snomed_ct|der_ec_|nosis_all"),
      starts_with("p_ec_") | starts_with("p_der_ec")
      ) |> 
    select(-c(valid, n))
}


df_model_specs_prep <- calc_perc_na(der_ec_diagnosis_all) |> 
  left_join(calc_perc_na(ec_acuity_snomed_ct), join_by(der_provider_code)) |> 
  left_join(calc_perc_na(ec_chief_complaint_snomed_ct), join_by(der_provider_code)) |> 
  left_join(calc_perc_na(ec_arrival_mode_snomed_ct), join_by(der_provider_code)) |> 
  left_join(calc_perc_na(ec_attendance_source_snomed_ct), join_by(der_provider_code)) |> 
  rename(
    p_complnt = p_chief_complaint,
    p_arrmode = p_arrival_mode,
    p_refsorc =p_attendance_source
    ) |> 
  # mutate(across(starts_with("p"), ~ median(., na.rm = T)))
  mutate(mod_spec = case_when(
    p_diag >= 0.85 & p_acuity >= 0.85 & p_arrmode >= 0.85 & p_refsorc >= 0.85 ~ "A", # FULL MODEL
    p_diag >= 0.80 & p_acuity >= 0.85 ~ "B", # LOWER DIAG THRESHOLD, REMOVE ARRIVAL AND REF SORCE
    p_complnt >= 0.85 & p_acuity >= 0.85 ~ "C", # COMPLAINT AND ACUITY
    p_arrmode >= 0.80 ~ "D", # ARRIVAL MODE
    TRUE ~ "E" # AGE SEX
  )) |> 
  # count(mod_spec)
  identity()
  
# df_model_specs_prep |> filter(mod_spec == "D")
  
df_model_specs <- df_model_specs_prep |> 
  select(der_provider_code, mod_spec) |> 
  mutate(formula = case_when(
    mod_spec == "A" ~ "admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + diag01 + arrmode_ambulance + acuity + referral_source + wkend + night_time_8to8",
    mod_spec == "B" ~ "admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + diag01 + acuity + wkend + night_time_8to8",
    mod_spec == "C" ~ "admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + complaint + acuity + wkend + night_time_8to8",
    mod_spec == "D" ~ "admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + arrmode_ambulance + wkend + night_time_8to8",
    mod_spec == "E" ~ "admitted ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + wkend + night_time_8to8",
    T ~ NA_character_
  ))


# USE R1H AS TEST CASE FOR LESS GOOD DQ

# ________----

param_provider_choice <- "RYR" # SUSSEX
# param_provider_choice <- "RC9" # BEDFORD

# 4. PREP 1----------------------------------------------------------------------

df_1 <- df_ecds_raw |> 
  filter(der_provider_code == param_provider_choice) |>
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

# ________----


## a. ENCODING -------------------------------------------------------------

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

df_encoding_part_1_of_4 <- df_1 |>
  left_join(lkp_diag, join_by(diag01_code))
  

### ii. chief complaint -----------------------------------------------------------
# options(scipen = 999)

# TODO IF WE WANTED TO IMPROVE MODEL, THERE ARE CERTAIN COMPLAINTS
# WITH A VERY HIGH CHANCE OF ADMISSION. MAKE SURE THESE ARE UNGROUPED.
# ALSO APPLICABLE TO DIAGNOSIS CODES.

# df_encoding_part_3_of_4 |> 
#   count(ref_chief_complaint, admitted, sort = T) |> 
#   group_by(ref_chief_complaint) |> 
#   mutate(n_total = sum(n)) |> 
#   mutate(p = n/sum(n)) |> 
#   ungroup() |> 
#   mutate(rank = dense_rank(desc(n_total))) |>
#   complete(ref_chief_complaint, admitted) |> 
#   rename(p_adm = p) |> 
#   filter(admitted == 1) |> 
#   arrange(-p_adm) |> 
#   mutate(p = n_total/sum(n_total, na.rm = T)) |> 
#   # mutate(csp = cumsum(p)) |>
#   print(n=30)

# tmp_diag_l3 <- tmp_diag_prep_l3 |>
#   slice(1:30) |>
#   select(diag01_code, diag01 = diag_descr_snomed, n, p, cs) |>
#   mutate(diag01 = janitor::make_clean_names(diag01)) |> 
#   mutate(diag01 = str_c("l3_", diag01))

tmp_complaint_prep <- df_encoding_part_1_of_4 |> 
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

df_encoding_part_2_of_4 <- df_encoding_part_1_of_4 |>
  left_join(lkp_complaint, join_by(ec_chief_complaint_snomed_ct))

### iii. other variables -----------------------------------------------------------

df_encoding_part_3_of_4 <- df_encoding_part_2_of_4 |>
  mutate(acuity = str_remove_all(ref_acuity, " level emergency care")) |>
  mutate(acuity = if_else(is.na(acuity), "NA", acuity)) |>
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
  # QUESTION ONLY EMERGENGY AMBULANCES ??
  mutate(arrmode_ambulance = case_when(
    ref_arrmode %in% c("Arrival by emergency road ambulance", "Arrival by non-emergency road ambulance", "Arrival by emergency road ambulance with medical escort") ~ "1",
    is.na(ref_arrmode) ~ "0",
    TRUE ~ "0"
  )) |> 
    # ref_arrmode == "Arrival by public transport" ~ "public_trans",
  left_join(lkp_ethref_raw, join_by(ethnic_category == code)) |>
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

# TODO RE-RUN WHERE CLAUSE SQL SCRIPT
readRDS(here("data", "where_exclusion_matrix.rds")) |> 
  filter(procode == param_provider_choice) |> 
  mutate(sigma = sum(n)) |>
  rowwise() |> 
  mutate(row_sum = sum(c_across(c(everything(), -c(procode, n, p, sigma))))) |> 
  ungroup() |> 
  # REMOVE THE ROW CORRESPONDING TO ATTENDANCES FOLLOWING ALL EXCLUSIONS:
  filter(row_sum != 0) |> 
  mutate(n_after_excl = sigma - cumsum(n)) |> 
  mutate(n_before_excl = n_after_excl + n) |> 
  select(-c(sigma, p)) |> 
  rename(n_excl = n) |> 
  relocate(c(n_before_excl, n_excl, n_after_excl), .after = row_sum) |> 
  # print(n=40)
  view("where")


## b. date-related ---------------------------------------------------------

# TIMESERIES OF LAST THREE MONTHS TO AID SELECTION OF CUT-OFF DATE:
df_encoding_part_3_of_4 |> 
  count(date_dep = date(der_ec_departure_date_time)) |> 
  filter(date_dep >= max(date_dep) - months(3)) |> 
  mutate(wkday = as.factor(wday(date_dep, label = T, week_start = 1))) |> 
  ggplot() +
  geom_point(aes(date_dep, n, col = wkday)) +
  geom_line(aes(date_dep, n), linewidth = 0.4)+
  # geom_smooth(aes(date_dep, n), se = F) +
  scale_color_brewer()+
  theme_minimal() +
  geom_blank(aes(y=0))+
  scale_x_date(date_breaks = "2 weeks")+
  NULL

# TODO THEN CHOOSE:
# OFFER ONLY SUNDAY DATES (WEEK ENDING) TO CHOOSE FROM
# AS IT HAPPENS LAST DAY OF AUG WAS SUNDAY.

# ________----


# 6. PREP 2 --------------------------------------------------------

# BASED ON ASSESSMENT OF ABOVE DQ:
param_date_cutoff <- "2025-08-31"
# WE MIGHT WANT A DEFAULT 
# (NOTE BY 5TH SEPTEM AUG SEEMS COMPLETE FOR SOME PROVS)

# as_date(yearweek(param_date_cutoff)) - days(1)


# (REQUIRED BEFORE MOVING TO FURTHER DQ CHECKS)

df_encoding_part_4_of_4 <- df_encoding_part_3_of_4 |> 
  ##### DATE RELATED SELECTIONS:
  filter(year_week <= yearweek(param_date_cutoff)) |> 
  # THE 7TH JAN ...
  filter(year_week >= yearweek("2025-01-07")) |> 
  #####
  mutate(across(c(everything(), -age), ~ as.factor(.))) |>
  # SET REF CATEGORY FOR DIAGNOSIS and OTHER CATS: (HIGH VOLUME, MODERATE EFFECT)
  mutate(diag01 = fct_relevel(diag01, "l3_lower_respiratory_tract_infection")) |> 
  mutate(acuity = fct_relevel(acuity, "Standard")) |> 
  mutate(ethnic_grp_sus = fct_relevel(ethnic_grp_sus, "British, Mixed British")) |> 
  mutate(referral_source = fct_relevel(referral_source, "self")) |> 
  # SET REF CATEGORY FOR YEAR_MONTH (SECOND WEEK OF CALENDAR YEAR):
  mutate(year_week = fct_relevel(year_week, as.character(yearweek("2025-01-08")))) |>
  identity()


# levels(df_encoding_part_4_of_4$year_week)
# levels(df_model_prep$diag01)
# df_model_prep |> glimpse()

# ________----

# 7. DATA QUAL 2-------

## c. regressor dq ----------------------------------------------------

# TODO CONTINGENT ON SELECTION OF DATE
# TODO AS ABOVE - SEPTEMBER REMOVED AS UNFINISHED.

df_encoding_part_4_of_4 |> 
  select(c(
    ethnic_grp_sus,
    imd_quint,
    diag01,
    ec_acuity_snomed_ct,
    ec_arrival_mode_snomed_ct,
    ec_attendance_source_snomed_ct
  )) |>
  names() |>
  map(
    ~ count(df_encoding_part_3_of_4, .data[[.x]], sort = T) |>
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


# DIAGNOSIS CODING: DIAGNOSTICS
# (could write tests here)

# tmp_diag_prep_l3 |> filter(diag_descr_snomed == "No abnormality detected") |> select(diag_descr_snomed, p)
# tmp_diag_prep_l3 |> filter(is.na(diag_descr_snomed))

# tmp_diag_l3 |> summarise(perc_coverage_l3 = max(cs))
# tmp_diag_l2 |> summarise(perc_coverage_l2 = sum(p))
# tmp_diag_l1 |> summarise(perc_coverage_l1 = sum(p))

# lkp_diag |> count(str_sub(diag01, 1, 2), diag01) |> nrow() == 52



# ________----

# 7. MODEL  -----------------------------------------------------------

# df_model_specs

# test_formula <- tibble(spec = list(formula(admitted ~ acuity)))
# 
# as.formula("ace ~ base")
# 
# test_formula |> unnest(spec)

vec_model_spec <- df_model_specs |> 
  filter(der_provider_code == param_provider_choice) |> 
  pull(formula)

mod <- mgcv::gam(
  formula = as.formula(vec_model_spec),
  family = "binomial",
  # method = "REML",
  data = df_encoding_part_4_of_4 
) 

# test_mod |> broom::tidy(parametric = T) |> 
#   mutate(odds = exp(estimate))

# mod <- mgcv::gam(
#   formula = admitted ~ 
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
#   # assign(str_c("mod_", param_provider_choice), value = _)
#   identity()

# mod <-  get(str_c("mod_", param_provider_choice))

saveRDS(mod, here("data", str_c("tmp_mod_wk_", param_provider_choice,".rds")))

mod <- readRDS(here("data", str_c("tmp_mod_wk_", param_provider_choice,".rds")))
# ________----

# 8. RESULTS --------------------------------------------------------------

# mod |> broom::glance()

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

# week(1)

# (WEEK BEGINNING)
lkp_week <- seq(ymd("2025-01-06"), ymd("2025-12-31"), by = "1 week") |> 
  enframe(name = NULL, value = "date") |> 
  mutate(term = yearweek(date)) |> 
  mutate(term = str_c("year_week", term))
  

# NEW ---------------------------------------------------------------------

prep_xmr <- results |> 
  select(term, estimate) |> 
  filter(str_detect(term, "year_week")) |>
  left_join(lkp_week, join_by(term)) |> 
  add_row(estimate = 0, date = as_date("2025-01-06")) |> 
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
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_y_log10()+
  theme(axis.title = element_text(size = 9))+
  scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
  labs(
    x = "\nDate",
    y = str_c(
      "Casemix-adjusted odds of admission (",
      param_provider_choice, ")\n(Reference category: w/c Jan 6th 2025)\n"
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
  theme(axis.title = element_text(size = 9))+
  scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
  labs(
    x = "\nDate",
    y = str_c(
      "Casemix-adjusted odds of admission (",
      param_provider_choice, ")\n(Reference category: w/c Jan 6th 2025)\n"
    )
  )
  
library("patchwork")
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
#     param_provider_choice, ")\n(Reference category: May 2025)\n"
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
