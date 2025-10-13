# README
# Process the raw ECDS data in readiness for model fitting. 


# 0. LOAD -----------------------------------------------------------------

# source(here::here("R", "02_load.R"))

gc()
gc()

source(here("R", "funs.R"))
source(here("R", "model_specifications.R"))
source(here("R", "provider_dq.R"))
source(here("R", "provider_dq_groups.R"))

# CUT OFF DATE IS MOST RECENT SUNDAY MINUS THREE WEEKS:
param_date_cutoff <- set_cutoff_date(min_buffer_days = 21)


# 1. JOIN TO REFERENCE TABLES ---------------------------------------------

df_ecds_joined <- df_ecds_raw |>
  mutate(diag01_code = str_extract(der_ec_diagnosis_all, "^[^,]*")) |>
  ### JOIN TO REFERENCE DF FOR TEXT DESRIPTION OF SNOMED CODES:
  left_join(df_ref_trimmed, join_by(ec_arrival_mode_snomed_ct == snomed_code)) |>
  rename(ref_arrmode = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_attendance_source_snomed_ct == snomed_code)) |>
  rename(ref_attsrc = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_discharge_status_snomed_ct == snomed_code)) |>
  rename(ref_disstat = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(discharge_destination_snomed_ct == snomed_code)) |>
  rename(ref_disdest = derived_snomed_descr) |>
  left_join(df_ref_trimmed, join_by(ec_acuity_snomed_ct == snomed_code)) |>
  rename(ref_acuity = derived_snomed_descr)

gc()
gc()


# 2. ASSIGN SPECS ---------------------------------------------------------

# 132 SITES = "GOOD"
# (BUT OF WHICH 7? SITES INCOMPLETE TIMESERIES)
# 47 SITES = "POTENTIAL / REAL ISSUES"

df_providers_nested <- df_ecds_joined |> 
  group_by(der_provider_site_code) |> 
  nest() |> 
  ungroup()

gc()

# TODO HERE I THINK WE'D ASSIGN SPECS BASED ON MEETING SOME CRITERIA
# THIS IS EXAMPLE:

df_providers_nested_specs <- bind_rows(
  df_providers_nested |>
    # BETTER DQ:
    filter(
      !der_provider_site_code %in% vec_shaky_providers
    ) |>
    cross_join(
      lkp_model_specs |>
        filter(mod_spec %in% c("a", "a1"))
    ),
  df_providers_nested |>
    # WORSE DQ:
    filter(
      der_provider_site_code %in% vec_shaky_providers
    ) |>
    cross_join(
      lkp_model_specs |>
        filter(mod_spec %in% c("b", "b1"))
    )
)

# 3. ENGINEER VARIABLES ------------------------------------------------
  
df_var_engineering_1of2 <- df_providers_nested_specs |> 
  mutate(data = map2(data, mod_spec, function(df, x) {
    
    if (x %in% c("a", "a1")) {
      ### i. diagnosis -----------------------------------------------
      
      tmp_diag_prep_l3 <- df |>
        left_join(df_ref_diagnosis, join_by(diag01_code == snomed_code)) |>
        count(diag01_code, diag_descr_snomed, ecds_group3, ecds_group2, ecds_group1, sort = T) |>
        mutate(p = n / sum(n)) |>
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
      
      return(
        df |>
          left_join(lkp_diag, join_by(diag01_code)) |>
          # SET REF CATEGORY FOR DIAGNOSIS: (HIGH VOLUME, MODERATE EFFECT)
          mutate(diag01 = fct_relevel(diag01, "l3_lower_respiratory_tract_infection")) |> 
          mutate(na_diag = if_else(diag01 %in% c("l3_na", "l2_na", "l1_other_engineered"), "1", "0"))
        
      )
    } else if (x %in% c("b", "b1")){
      ### ii. chief complaint -----------------------------------------------------------
      
      tmp_complaint_prep <- df |>
        count(ec_chief_complaint_snomed_ct, sort = T) |>
        left_join(df_ref, join_by(ec_chief_complaint_snomed_ct == snomed_code)) |>
        select(-n, everything(), n, -c(ecds_group3, ecds_group2)) |>
        mutate(p = n / sum(n)) |>
        mutate(cs = cumsum(p))
      
      tmp_complaint_l2 <- tmp_complaint_prep |>
        slice(1:50) |>
        select(ec_chief_complaint_snomed_ct, complaint = derived_snomed_descr) |>
        mutate(complaint = janitor::make_clean_names(complaint)) |>
        mutate(complaint = str_c("l2_", complaint))
      
      tmp_complaint_l1 <- tmp_complaint_prep |>
        slice(-c(1:50)) |>
        select(ec_chief_complaint_snomed_ct, complaint = ecds_group1) |>
        mutate(complaint = janitor::make_clean_names(complaint, allow_dupes = T)) |>
        mutate(complaint = str_c("l1_", complaint))
      
      # NOT PROVIDER SPECIFIC:
      lkp_complaint <- bind_rows(
        tmp_complaint_l2,
        tmp_complaint_l1
      )
      
      return(
        df |>
          left_join(lkp_complaint, join_by(ec_chief_complaint_snomed_ct)) |>
          mutate(complaint = fct_relevel(complaint, "l2_chest_pain")) |> 
          mutate(na_complaint = if_else(complaint %in% c("l2_na", "l1_na"), "1", "0"))
        
      )
    }
  }
           
           
           ))

gc()
gc()

# NOTE: SOME SITES DON'T CODE CHEST PAIN SO CAN'T BE REFERENCE LEVEL HERE.
# df_var_engineering_1of2$data[[2]] |> 
#   count(complaint, sort = T)

# NOTE: OPTION TO REVERT TO DISTAT 1324201000000109 (ED TO IP) IF NA DISDEST

df_var_engineering_2of2 <- df_var_engineering_1of2 |> 
  mutate(data = map(data, function(df){
        df |>
      mutate(acuity = str_remove_all(ref_acuity, " level emergency care")) |>
      mutate(acuity = if_else(is.na(acuity), "NA", acuity)) |>
      mutate(ed_discharged = case_when(
        ref_disdest %in% c(
          "Discharge to ward",
          "Emergency department discharge to coronary care unit",
          "Emergency department discharge to high dependency unit",
          "Emergency department discharge to intensive care unit",
          "Emergency department discharge to operating theatre",
          "Emergency department discharge to neonatal intensive care unit",
          "Emergency department discharge to special care baby unit"  
        ) ~ 0,
        # is.na(ref_disdest) & ec_discharge_status_snomed_ct == "1324201000000109" ~ 0,
        is.na(ref_disdest) ~ 1,
        TRUE ~ 1
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
      )) |> 
      ##### DATE RELATED SELECTIONS:
      filter(year_week <= yearweek(param_date_cutoff)) |> 
      # WE START FROM FIRST MONDAY OF THE YEAR.
      # THE 7TH JAN WILL ALWAYS BE IN THE WEEK WE WANT TO START WITH.
      filter(year_week >= yearweek("2025-01-07")) |> 
      ####
    ## EXCLUDING DIAG AND COMPLAINT AS REF LEVEL ALREADY SPECIFIED:
    mutate(across(c(everything(), - age, -matches("diag01$"), -matches("complaint$")), ~ as.factor(.))) |>
      # EXPLICITLY SET REF LEVEL FOR OUTCOME (NECESSARY FOR CONSISTENT METRICS):
      mutate(ed_discharged = fct_relevel(ed_discharged, "0")) |> 
      # SET REF LEVEL FOR OTHER CATS: (HIGH VOLUME, MODERATE EFFECT)
      mutate(acuity = fct_relevel(acuity, "Standard")) |> 
      mutate(ethnic_grp_sus = fct_relevel(ethnic_grp_sus, "British, Mixed British")) |> 
      # mutate(referral_source = fct_relevel(referral_source, "self")) |> 
      # SET REF CATEGORY FOR YEAR_MONTH (SECOND WEEK OF CALENDAR YEAR):
      mutate(year_week = fct_relevel(year_week, as.character(yearweek("2025-01-08")))) |>
      identity()
    
  }))

gc()
gc()

  
# ! ISSUE ! ------------------------
# WITH DISCHARGE DESTINATION - SOME PROVIDERS HAVE FEW/NO ADMISSIONS 

# df_var_engineering_2of2$data[[39]] |> count(ed_discharged)
# df_var_engineering_2of2$data[[39]] |> count(ref_disdest, discharge_destination_snomed_ct)
# 
# df_var_engineering_2of2 |> 
#   slice(c(3, 14, 20, 10, 21, 22)) |>
#   mutate(site_name = map_chr(data, \(df) df |> distinct(site_name) |> pull() |> as.character())) |> 
#   distinct(der_provider_site_code, site_name)
#   
#   
# df_var_engineering_2of2 |> 
#   # slice(-c(3, 14, 20, 22)) |>
#   mutate(p = map_dbl(data, function(df) {
#     df |> 
#       count(ed_discharged) |>
#       mutate(p = n/sum(n)) |> 
#       filter(ed_discharged == 1) |> 
#       pull(p)-1 
#   }
#   )) |> 
#   mutate(p_adm = abs(p)) |> 
#   mutate(site_name = map_chr(data, \(df) df |> distinct(site_name) |> pull() |> as.character())) |> 
#   distinct(der_provider_site_code, site_name, p_adm) |> 
#   arrange(p_adm) |> 
#   print(n= 110)
# 
# # TODO PROBLEM SITES WITH LOW % ADMISSIONS
# #    der_provider_site_code site_name                                                p_adm
# #    <chr>                  <chr>                                                    <dbl>
# #  1 RA201                  Royal Surrey County Hospital                          0.000720
# #  2 RQWG0                  The Princess Alexandra Hospital                       0.0182  
# #  3 RQ311                  Birmingham Childrens Hospital - Accident & Emergency  0.0463  
# #  4 RN707                  Darent Valley Hospital                                0.0868  
# #  5 RJ611                  Croydon University Hospital                           0.0925  
# #  6 RBS25                  Royal Liverpool Childrens Hospital                   0.0970  
# #  7 RQXM1                  Homerton University Hospital                          0.0995  
# #  8 RR801                  Leeds General Infirmary                               0.115   
# #  9 RKB01                  University Hospital Coventry                          0.119   
# # 10 RJN71                  Macclesfield District General Hospital                0.132   
# # 11 RQM01                  Chelsea and Westminster Hospital                      0.139   
# # 12 RQM91                  West Middlesex University Hospital                    0.143   
# 
# 
# 
# df_var_engineering_2of2 |> 
#   filter(der_provider_site_code == "RA201") |>
#   unnest(data) |> 
#   # colnames()
#   count(ref_disdest, ec_discharge_status_snomed_ct, sort = T) |> 
#   print(n=40)


# end issue-------------------------------------------------------------------------
