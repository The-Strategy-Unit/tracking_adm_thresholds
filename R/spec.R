# README 
# Model specification options and data quality thresholds 


# MODEL SPECIFICATION PERMETATIONS:

base_spec <- 
  "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + wkend + night_time_8to8"

lkp_model_specs <- 
# POSSIBLE ADDITIONAL COMPONENTS CONTINGENT ON DATA QUALITY:
  c(
    a = " + diag01 + arrmode_ambulance + acuity + referral_source",
    b = " + diag01 + arrmode_ambulance",
    c = " + complaint + acuity",
    d = " + arrmode_ambulance",
    e = ""
) |> 
  enframe(
    name = "mod_spec",
    value = "mod_formula"
    ) |> 
  mutate(mod_formula = str_c(base_spec, mod_formula)) 


# DATA QUALITY THRESHOLDS FOR KEY VARIABLES (TO DETERMINE MODEL SPECS):

df_thresholds <- tribble(
  ~mod_spec, ~min_diag, ~min_complnt, ~min_acuity, ~min_arrmode, ~min_refsorc,
  "a", 0.85, 0, 0.85, 0.85, 0.85,
  "b", 0.80, 0, 0, 0.85, 0,
  "c", 0, 0.8, 0.80, 0, 0,
  "d", 0, 0, 0, 0.85, 0,
  "e", 0, 0, 0, 0, 0,
) |>
  mutate(rank = match(mod_spec, letters))

