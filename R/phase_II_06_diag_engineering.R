# TODO FRI 28 NOV:
# ADD FINAL ENGINEERING 
# REVIEW THEORY
# EDA 
# MODEL


# 41 % IN L3
df_diag_prep_l3 <- df_ecds_II_sample |>
  left_join(df_ref_diagnosis, join_by(diag01_code == snomed_code)) |>
  count(diag01_code, diag_descr_snomed, ec_chief_complaint_snomed_ct, sort = T, name = "nn") 

# THEN WE NEED A NEW STRATEGY TO GET AROUND THE "CODE DEPRECATED" ISSUE 
# IN THE REGULAR GROUPING SCHEME: SO USING COMPLAINT: 
df_diag_complaint_prep_l2_l1 <- df_diag_prep_l3 |>
  group_by(diag01_code) |> 
  mutate(n =  sum(nn)) |>
  ungroup() |> 
  mutate(p = n / sum(nn)) |> 
  arrange(-p) |> 
  group_by(-n) |> 
  mutate(r = cur_group_id()) |> 
  ungroup() |> 
  filter(r > 30) |> 
  count(ec_chief_complaint_snomed_ct, wt = nn, sort = T) |> 
  left_join(df_ref, join_by(ec_chief_complaint_snomed_ct == snomed_code)) 

# we have 1 run with diag, then anything not matched goes to complaint

df_diag_l3_1of2 <- df_diag_prep_l3 |> 
  count(diag01_code, diag_descr_snomed, wt = nn, sort = T) |> 
  slice(1:30) |> 
  mutate(diag = janitor::make_clean_names(diag_descr_snomed)) |>
  # mutate(diag = janitor::make_clean_names(diag_descr_snomed, allow_dupes = T)) |>
  mutate(diag = str_c("l3_", diag)) |> 
  select(-c(n, diag_descr_snomed)) 


# L3 DIAGNOSES OVER TIME (PLOT) -------------------------------------------

# df_diag_l3_1of2
  

df_ecds_II_sample |>
  # # RECODE DISCONTINUED "REFERRAL TO SERVICE" AS NA
  # mutate(diag01_code = if_else(diag01_code == "306206005", NA_character_, diag01_code)) |>
  # # RECODE DISCONTINUED UTI AS UT INFECTIOUS DISEASE
  # mutate(diag01_code = if_else(diag01_code == "68566005", "4009004", diag01_code)) |>
  inner_join(df_diag_l3_2of2, join_by(diag01_code)) |>
  count(year_quarter, diag) |>
  # filter(as_date(year_quarter) < as_date("2025-07-01")) |>
  #   count(diag, diag01_code, wt = n, sort = T) |>
  # print(n=32)
  # filter(is.na(diag_descr_snomed))
  # mutate(diag = if_else(diag01_code == "", "none", diag)) |>
  ggplot(aes(as_date(year_quarter), n, group = diag, col = diag))+
  geom_line()+
  geom_point(size = 0.6)+
  geom_blank(aes(y = 0))+
  # facet_wrap(vars(diag_descr_snomed))+
  facet_wrap(vars(diag), scales = "free_y")+
  theme_minimal()+
  theme(
    strip.text = element_text(size = 5.5),
    legend.position = "none",
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
  )

#  -------------------------------------------------------------------------

# AS INDICATED BY THE GRAPHIC ABOVE:
# RECODE THE DISCONTINUED "REFERRAL TO SERVICE" LABEL AS NA:
# RECODE DISCONTINUED UTI AS UT INFECTIOUS DISEASE:
lkp_diag_II <- df_diag_l3_1of2 |> 
  mutate(diag = case_when(
    diag01_code == "306206005" ~ "l3_na",
    diag01_code == "68566005" ~ "l3_lower_urinary_tract_infectious_disease",
    T ~ diag
  ))
  


# NOW COMPLAINT ---------------------------------------------------------------

# BASED ON THE GRAPHIC BELOW AND SOME RESEARCH, WE'RE GOING TO
# GROUP SPECIFIC INJURIES OF EXTREMITIES UNDER DISCONTIUED "INJURY OF X EXTREMITY" LABEL
df_complaint_recode <- df_diag_complaint_prep_l2_l1 |>
  filter(str_detect(derived_snomed_descr, "njury of")) |> 
  filter(str_detect(derived_snomed_descr, "ankle|foot|knee|leg|thigh|toe|elbow|finger|forearm|hand|upper arm|wrist")) |> 
  distinct(ec_chief_complaint_snomed_ct, derived_snomed_descr) |> 
  mutate(extremity = case_when(
    str_detect(derived_snomed_descr, "ankle|foot|knee|leg|thigh|toe") ~ "Injury of lower extremity",
    str_detect(derived_snomed_descr, "elbow|finger|forearm|hand|upper arm|wrist") ~ "Injury of upper extremity",
    T ~ derived_snomed_descr
  )) 
  

# 79% COVERED BY L1 AND L2
df_complaint_l2_1of2 <- df_diag_complaint_prep_l2_l1 |>
  filter(
    row_number() %in% 1:20 | ec_chief_complaint_snomed_ct %in% df_complaint_recode$ec_chief_complaint_snomed_ct
  ) |>
  select(ec_chief_complaint_snomed_ct, derived_snomed_descr) |>
  mutate(derived_snomed_descr = if_else(
    ec_chief_complaint_snomed_ct %in% (
      df_complaint_recode |>
        filter(extremity == "Injury of upper extremity") |>
        pull(ec_chief_complaint_snomed_ct)),
    "Injury of upper extremity",
    derived_snomed_descr
  )) |>
  mutate(derived_snomed_descr = if_else(
    ec_chief_complaint_snomed_ct %in% (
      df_complaint_recode |>
        filter(extremity == "Injury of lower extremity") |>
        pull(ec_chief_complaint_snomed_ct)),
    "Injury of lower extremity",
    derived_snomed_descr
  )) |>
  # MAKE SURE CHIEF COMPLAINT NA IS DIFFERENT TO DIAGNOSIS NA:
  mutate(diag = janitor::make_clean_names(derived_snomed_descr, allow_dupes = TRUE)) |>
  mutate(diag = if_else(is.na(ec_chief_complaint_snomed_ct), "na_complaint", diag)) |>
  # mutate(diag = janitor::make_clean_names(diag_descr_snomed, allow_dupes = T)) |>
  mutate(diag = str_c("l2_", diag)) |>
  select(-c(derived_snomed_descr))

# df_complaint_l2 |> 
#   print(n=40)

# L2 COMPLAINT OVER TIME ------------------------------------------------

# df_ecds_II_sample |> 
#   anti_join(df_diag_l3_2of2, join_by(diag01_code)) |> 
#   # nrow()
#   inner_join(df_complaint_l2, join_by(ec_chief_complaint_snomed_ct)) |>
#   count(year_quarter, diag) |>
#   # filter(as_date(year_quarter) < as_date("2025-07-01")) |>
#   #   count(diag, diag01_code, wt = n, sort = T) |>
#   # print(n=32)
#   # filter(is.na(diag_descr_snomed))
#   ggplot(aes(as_date(year_quarter), n, group = diag, col = diag))+
#   geom_line()+
#   geom_point(size = 0.6)+
#   geom_blank(aes(y = 0))+
#   # facet_wrap(vars(diag_descr_snomed))+
#   facet_wrap(vars(diag), scales = "free_y")+
#   theme_minimal()+
#   theme(
#     strip.text = element_text(size = 5.5),
#     legend.position = "none",
#     axis.text = element_text(size = 5),
#     axis.title = element_text(size = 5),
#   )

# df_complaint_l2 |> 
#   bind_rows()


# perhaps lower limb / upper limb should be reassigned?

 # df_ecds_II_sample |>
 #  count(year_quarter, ec_chief_complaint_snomed_ct) |>
 #  # inner_join(df_freq_complaints, join_by(ec_chief_complaint_snomed_ct)) |>
 #  inner_join(
 #    df_ref |> 
 #      filter(str_detect(derived_snomed_descr, "njury of")),
 #    join_by(ec_chief_complaint_snomed_ct == snomed_code)
 #  ) |> 
 #  filter(as_date(year_quarter) < as_date("2025-07-01")) |>
 #  mutate(derived_snomed_descr = case_when(
 #    str_detect(derived_snomed_descr, "ankle|foot|knee|leg|thigh|toe") ~ "Injury of lower extremity",
 #    str_detect(derived_snomed_descr, "elbow|finger|forearm|hand|upper arm|wrist") ~ "Injury of upper extremity",
 #    T ~ derived_snomed_descr
 #  )) |> 
 #  count(year_quarter, derived_snomed_descr, wt = n) |> 
 #  # filter(is.na(diag_descr_snomed))
 #  # mutate(diag_descr_snomed = if_else(diag01_code == "", "none", diag_descr_snomed)) |>
 #  ggplot(aes(as_date(year_quarter), n, group =  derived_snomed_descr, col =  derived_snomed_descr))+
 #  geom_line()+
 #  geom_point(size = 0.6)+
 #  geom_blank(aes(y = 0))+
 #  facet_wrap(vars(derived_snomed_descr))+
 #  # facet_wrap(vars( derived_snomed_descr), scales = "free_y")+
 #  theme_minimal()+
 #  theme(
 #    strip.text = element_text(size = 5.5),
 #    legend.position = "none",
 #    axis.text = element_text(size = 5),
 #    axis.title = element_text(size = 5),
 #  )



# END L2 COMPLAINT OVER TIME ------------------------------------------------

df_complaint_l1_1of2 <- df_diag_complaint_prep_l2_l1 |> 
  filter(
    ! (row_number() %in% 1:20 | ec_chief_complaint_snomed_ct %in% df_complaint_recode$ec_chief_complaint_snomed_ct)
  ) |>
  mutate(ecds_group1 = case_when(
    derived_snomed_descr == "Unsteady gait" ~ "Neurological", # *  Musculoskeletal / General
    derived_snomed_descr == "Gestation less than 20 weeks" ~ "ObGyn", 
    derived_snomed_descr == "Earache symptom" ~ "Head and neck", 
    derived_snomed_descr == "Vaginal bleeding" ~ "ObGyn",
    derived_snomed_descr == "Blood in faeces symptom" ~ "Gastrointestinal", 
    derived_snomed_descr == "Wound care" ~ "General", 
    derived_snomed_descr == "Traumatic injury" ~ "Injury",
    derived_snomed_descr == "Speech dysfunction" ~ "Neurological",
    derived_snomed_descr == "Red eye" ~ "Eye",
    derived_snomed_descr == "Complaining of feeling depressed" ~ "Psychosocial / Behaviour change",
    derived_snomed_descr == "Oliguria" ~ "Genitourinary", # *?
    derived_snomed_descr == "Unilateral leg oedema" ~ "Circulation / chest", # *?
    derived_snomed_descr == "Social problem" ~ "General", 
    derived_snomed_descr == "Polyuria" ~ "Genitourinary", # *?
    derived_snomed_descr == "Gestation greater than 20 weeks" ~ "ObGyn",
    derived_snomed_descr == "Toxic effect of gas, fumes AND/OR vapours" ~ "Environmental", 
    derived_snomed_descr == "Drowsy" ~ "Neurological", # *?
    derived_snomed_descr == "Collapse" ~ "Circulation / chest", # *?
    derived_snomed_descr == "Pale complexion" ~ "General", # *?
    derived_snomed_descr == "At risk for injury due to fall" ~ "Neurological", # *?
    derived_snomed_descr == "Pain in testicle" ~ "Genitourinary", 
    derived_snomed_descr == "Lightheadedness" ~ "General", # *?
    derived_snomed_descr == "Swelling / lump finding" ~ "Skin", # *?
    derived_snomed_descr == "Swelling of lower leg" ~ "Circulation / chest", # *?
    derived_snomed_descr == "Post-surgical wound care" ~ "General", # *?
    derived_snomed_descr == "Foreign body in skin" ~ "Skin", # *?
    derived_snomed_descr == "Exposure to communicable disease" ~ "Environmental", # *?
    derived_snomed_descr == "Drug withdrawal" ~ "Drug and Alcohol",
    derived_snomed_descr == "Loss of consciousness" ~ "Circulation / chest", # *?
    derived_snomed_descr == "Unusual change in behaviour" ~ "Psychosocial / Behaviour change", # *?
    derived_snomed_descr == "Syncope and collapse" ~ "Circulation / chest", # *?
    derived_snomed_descr == "Swelling of inguinal region" ~ "Genitourinary", # *?
    derived_snomed_descr == "Unsteady when standing" ~ "Neurological", # *?
    derived_snomed_descr == "Paraesthesia" ~ "Skin", # *?
    derived_snomed_descr == "Difficulty swallowing food" ~ "Head and neck", # *?
    derived_snomed_descr == "Blue lips" ~ "General", # *?
    T ~ ecds_group1
  )) |> 
  select(ec_chief_complaint_snomed_ct, ecds_group1) |> 
  mutate(diag = janitor::make_clean_names(ecds_group1, allow_dupes = T)) |>
  mutate(diag = str_c("l1_", diag)) |> 
  select(-c(ecds_group1)) 

# df_complaint_l1 |> 
#   count(diag, sort = T)

# L1 OVER TIME ------------------------------------------------------------

# df_ecds_II_sample |>
#   anti_join(df_diag_l3_2of2, join_by(diag01_code)) |> 
#   anti_join(df_complaint_l2, join_by(ec_chief_complaint_snomed_ct)) |> 
#   # nrow()
#   inner_join(df_complaint_l1, join_by(ec_chief_complaint_snomed_ct)) |>
#   # count(year_quarter, diag) |>
#   # filter(as_date(year_quarter) < as_date("2025-07-01")) |>
#   count(year_quarter, diag, sort = T) |>
#   # print(n=32)
#   # filter(is.na(diag_descr_snomed))
#   ggplot(aes(as_date(year_quarter), n, group = diag, col = diag))+
#   geom_line()+
#   geom_point(size = 0.6)+
#   geom_blank(aes(y = 0))+
#   # facet_wrap(vars(diag_descr_snomed))+
#   facet_wrap(vars(diag), scales = "free_y")+
#   theme_minimal()+
#   theme(
#     strip.text = element_text(size = 5.5),
#     legend.position = "none",
#     axis.text = element_text(size = 5),
#     axis.title = element_text(size = 5),
#   )

# TIDY L2 AND L1 -----------------------------------------------------------

# DRUG AND ALC L1 TO SUBSTANCE MISUSE L2
df_complaint_l2_2of2 <- df_complaint_l2_1of2 |>   
  bind_rows(
    df_complaint_l1_1of2 |> 
      filter(str_detect(diag, "rug")) |> 
      mutate(diag = "l2_substance_misuse")
  ) |> 
  filter(!str_detect(diag, "limb"))
  
# LOWER LIMB AND UPPER LIMB L2 TO MSK L1
df_complaint_l1_2of2 <- df_complaint_l1_1of2 |>  # ---- follow the structure above
  bind_rows(
    df_complaint_l2_1of2 |> 
      filter(str_detect(diag, "limb")) |> 
      mutate(diag = "l1_musculoskeletal")
  ) |> 
  filter(!str_detect(diag, "rug")) 



# 15 more codes .. meaning 65 levels altogether
# NOT PROVIDER SPECIFIC:
lkp_complaint_II <- bind_rows(
  df_complaint_l2_2of2,
  df_complaint_l1_2of2
)

# 60 CONDITION CATEGORIES IN ALL
lkp_diag_II |> count(diag) # 28 ... 41%
lkp_complaint_II |> count(diag) # 32 ... (38% +21 %)


# CONDITION CATEGORIES OVER TIME ------------------------------------------

plot_condition_over_time <- function(df) {
  df |>
    ggplot(aes(as_date(year_quarter), n, group = diag, col = diag)) +
    geom_line() +
    geom_point(size = 0.6) +
    geom_blank(aes(y = 0)) +
    facet_wrap(vars(diag), scales = "free_y") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 5.5),
      legend.position = "none",
      axis.text = element_text(size = 5),
      axis.title = element_text(size = 5),
    )
}

df_preplot_condition_over_time <- tibble(
  level = c(3, 2, 1),
  data = list(
    # LEVEL 3
    df_ecds_II_sample |>
      inner_join(lkp_diag_II, join_by(diag01_code)) |>
      count(year_quarter, diag),
    # LEVEL 2
    df_ecds_II_sample |>
      anti_join(lkp_diag_II, join_by(diag01_code)) |>
      inner_join(df_complaint_l2_2of2, join_by(ec_chief_complaint_snomed_ct)) |>
      count(year_quarter, diag),
    # LEVEL 1
    df_ecds_II_sample |>
      anti_join(lkp_diag_II, join_by(diag01_code)) |>
      anti_join(df_complaint_l2_2of2, join_by(ec_chief_complaint_snomed_ct)) |>
      inner_join(df_complaint_l1_2of2, join_by(ec_chief_complaint_snomed_ct)) |>
      count(year_quarter, diag)
  )
)

df_preplot_condition_over_time |> saveRDS(here("data", "251204_dfq_condition_over_time.rds"))

# FOR ACUITY OVER TIME, SEE DATA PREP SCRIPT.


  plot_condition_over_time()
  plot_condition_over_time()
  plot_condition_over_time()

# -------------------------------------------------------------------------

gc()
gc()
gc()
gc()
