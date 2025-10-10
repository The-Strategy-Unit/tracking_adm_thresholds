# README 
# The range of model specifications. (The specification used for a given 
# provider is determined by that provider's data quality.)

library("dplyr")
library("tibble")
library("stringr")


base_spec <- 
  "ed_discharged ~ year_week + s(age, by = sex) + sex + imd_quint + ethnic_grp_sus + wkend + night_time_8to8"

lkp_model_specs <- 
# POSSIBLE ADDITIONAL COMPONENTS CONTINGENT ON DATA QUALITY:
  c(
    a = " + diag01 + arrmode_ambulance + acuity + referral_source",
    a1 = " + diag01 + arrmode_ambulance + acuity + referral_source + year_week:na_diag",
    # b = " + diag01 + arrmode_ambulance",
    # c = " + complaint + acuity",
    b = " + complaint + arrmode_ambulance + acuity + referral_source",
    b1 = " + complaint + arrmode_ambulance + acuity + referral_source + year_week:na_complaint"
    # d = " + arrmode_ambulance",
    # e = ""
) |> 
  enframe(
    name = "mod_spec",
    value = "mod_formula"
    ) |> 
  mutate(mod_formula = str_c(base_spec, mod_formula)) 

