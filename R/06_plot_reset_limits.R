# README
# This script was used to produce the "limit reset" graphics in the
# report. Also suggests the steps that may be required for auto
# reset of limits in any future tool. 

# NOTES:
# NOT INTERESTED IN SHOCKS BUT IN SUSTAINED CHANGE
# FOR APP:
# WOULD HAVE TO PROGRAMATICALLY RESET MULTIPLE TIMES
# WOULD NEED TO INTERACT WITH MOVING RANGE PLOT (IF LATTER INCLUDED).

library("here")
library("odbc")
library("mgcv")
library("broom")
library("dplyr")
library("purrr")
library("furrr")
library("readr")
library("tidyr")
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
library("NHSRplotthedots")

source(here("R", "funs_plot_the_dots.R"))


# i. setup ----------------------------------------------------------------



tmp <- readRDS(
  here(
    "data",
    str_c(
      "SHARED_", 29, "to", 42, "_results.rds"
    )
  )
)

# ID 30 = SALFORD ROYAL
tmp0 <- tmp |> filter(id == 30)

tmp_flag_nc <- tmp0 |> pull(data_flag) |> deframe()

tmp_prep_xmr <- tmp0 |> pull(prep_xmr) |> deframe()


# 1. DERIVE LIMITS AND LABEL POINTS: -------------

# TOTAL NUMBER OF WEEKS TO DISPLAY IN CHART
# (USED FOR GRAPHICS IN REPORT BUT UNNECESSARY OTHERWISE)
param_number_weeks <- 12


tmp_ptd <- tmp_prep_xmr |>
  # IF WISH TO TEST MULTIPLE REBASES WITH ARTIFICIAL DATA:
  # bind_rows(tmp_prep_xmr |> mutate(date = date + weeks(41))) |> 
  slice(1:param_number_weeks) |>
  ptd_spc(
    value_field = estimate,
    date_field = date,
    fix_after_n_points = 20
    # rebase = ptd_rebase(c(as_date("2025-09-01"))), # , as_date("2025-12-01")
  ) |>
# 2. RELABEL PTD POINTS ACCORDING TO OUR NEEDS: ------
  mutate(
    # FOR OUR PURPOSES "CAUSE TYPE REDUX" IS USED AFTER EVERY USE OF ptd_spc().
    # THE FUNCTION HERE USES A REVISED VERSION OF THE ORIGINAL PTD FUNCTION
    # AND WILL OVERWRITE ANY SPECIAL CAUSE TYPE LABELLED BY PTD PACKAGE -
    # PRIORITISING TRENDS OVER SHOCKS (ALSO REMOVING THE 2 IN 3 RULE):
    special_cause_type = ptd_special_cause_type_redux(
      y,
      relative_to_mean,
      close_to_limits,
      outside_limits
    )
  )
 

# 3. IF THESE ARE ANY COMPLETED 7 POINT SEQUENCES, GET THE DATES OF THESE:------
df_prep_rebase <- tmp_ptd |>
  mutate(rebase_dates = if_else(
    str_detect(special_cause_type, "7 Points Above Mean|7 Points Below Mean|7 Point Trend (Increasing)|7 Point Trend (Decreasing)"), 
    x, 
    NA_Date_
    )) 

# 4. KEEP THE MINIMUM REBASE DATE -------------------
tmp_min_date <- df_prep_rebase |> summarise(min(rebase_dates, na.rm = T)) |>  pull()

# 5. BASED ON EARLIEST OF REBASE DATES, OVERWRITE THE PREVIOUS LIMITS AND POINT LABELS. ----------
df_test <- df_prep_rebase %>%
  ptd_spc(
  value_field = y,
  date_field = x,
  # REBASE THE LIMITS BASED ON THE EARLIEST DATE IN THE COMPLETED SEQUENCE:
  rebase = with(data = ., ptd_rebase(min(rebase_dates, na.rm = T))), # , as_date("2025-12-01")
  fix_after_n_points = 20
) |> 
  as_tibble() 


# 6. save  ----------------------------------------------------------------
# df snapshots at various points:

df_test |> 
  mutate(x = as_date(x)) |> 
  left_join(tmp_flag_nc, join_by(x == date)) |>  
  # saveRDS(here("data", "260312_limit_reset_12_weeks.rds"))
  # saveRDS(here("data", "260312_limit_reset_26_weeks.rds"))
  # saveRDS(here("data", "260312_limit_reset_28_weeks.rds"))
  # saveRDS(here("data", "260312_limit_reset_40_weeks.rds"))
  identity()
  
  # readRDS(here("data", "260312_limit_reset_26_weeks.rds")) |> 
  readRDS(here("data", "260312_limit_reset_26_weeks.rds")) 
readRDS(here("data", "260312_limit_reset_13_weeks.rds")) |>


# PLOT --------------------------------------------------------------------
# THIS HAS BEEN FORMALISED IN FUNCTION IN REPORT.

readRDS(here("data", "260312_limit_reset_40_weeks.rds")) |>
  ggplot(aes(x, y)) +
  geom_point(aes(x, y, col = flag_risk))+
  geom_line(aes(x, mean_col, group = rebase_group), lty = "dashed") +
  geom_line(aes(x, lpl, group = rebase_group), lty = "dashed") +
  geom_line(aes(x, upl, group = rebase_group), lty = "dashed") +
  scale_color_manual(
    values = c("grey20", "indianred"),
    name = "Evidence of non-constant risks:",
    labels = c("No", "Yes")
  ) +
  geom_line(aes(x, y)) +
  theme_minimal() +
  scale_y_log10(
    limits = c(0.8, 1.8),
    labels = c(0.8, 1, 1.2, 1.4, 1.6 , 1.8),
    breaks = c(0.8, 1, 1.2, 1.4, 1.6 , 1.8)
    ) +
  theme(
    axis.title = element_text(size = 7),
    # legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_x_date(
    date_breaks = "month",
    date_labels = "%b %Y",
    limits = c(as_date("2025-04-07"), as_date("2026-01-12"))
    ) +
  labs(
    # title = str_c("Salford Royal", "\n"),
    # title = str_c(y, ". ", x)
    x = "\nDate",
    y = str_c(
      "Casemix-adjusted odds of admission (log scale)\n(Reference category: w/c April 7th 2025)\n"
    )
  )

######################
# SECOND ITERATION (FOR MULTIPLE LIMIT RESETS)
######################

# WOULD LIKELY REPEAT SOME OF THE STEPS ABOVE WITHIN WHILE LOOP?

