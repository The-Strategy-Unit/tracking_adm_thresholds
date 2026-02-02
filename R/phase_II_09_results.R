# README
# Get model stats and results.

mod_run_1 <- readRDS(here::here("data", "251203_mod_run_1.rds"))
mod_run_2 <- readRDS(here::here("data", "251205_mod_run_2_acuity.rds"))
mod_run_3 <- readRDS(here::here("data", "251205_mod_run_3_sans_acuity.rds"))
mod_run_4 <- readRDS(here::here("data", "251208_mod_run_4_covid.rds"))
mod_run_5 <- readRDS(here::here("data", "251208_mod_run_5_interaction.rds"))
mod_run_7 <- readRDS(here::here("data", "251211_mod_run_7_interaction.rds"))
mod_run_8 <- readRDS(here::here("data", "251212_mod_run_8_interactions.rds"))
mod_run_9 <- readRDS(here::here("data", "251215_mod_run_9_interaction.rds"))


# STATS -------------------------------------------------------------------
# head(fitted(mod_run_5))
head(mod_5_resample)

# TEST ON OUT OF SAMPLE RECORDS:
set.seed(1994)
newdata <- df_var_ref_levels |> 
  slice_sample(n = 2.5e6)

mod_5_resample <- predict(mod_run_5, newdata, type ="response")
mod_4_resample <- predict(mod_run_4, newdata, type ="response")
mod_7_resample <- predict(mod_run_7, newdata, type ="response")
mod_8_resample <- predict(mod_run_8, newdata, type ="response")
mod_9_resample <- predict(mod_run_9, newdata, type ="response")


tmp_stats <- tibble(
  obs = as.factor(mod_run_9$y),
  # obs = df_var_ref_levels_sample_1m$admitted,
  fit = 1 - fitted(mod_run_9), # predicted probabilities 1
  # fit = fitted(mod_run_1) # predicted probabilities 1
  # OUT OF SAMPLE RECORDS:
  # obs_oos = newdata$admitted,
  # fit_oos = 1 - mod_8_resample
) |> 
  # mutate(fit_oos = as.double(fit_oos)) |> 
  identity()



tmp_stats |> brier_class(obs, fit)
tmp_stats |> brier_class(obs_oos, fit_oos)
# tmp_stats |> brier_class(obs1, fit)
tmp_stats |> roc_auc(obs, fit)
tmp_stats |> roc_auc(obs_oos, fit_oos)
# tmp_stats |> roc_auc(obs, fit, event_level = "second")
# tmp_stats |> roc_auc(obs1, fit, event_level = "second")

# PREDICT NUMBERS AFFECTED -------------------------------------------------------------------

df_fixed_threshold <- df_var_ref_levels |> 
  mutate(year_quarter = as_factor("2020 Q1"))

# levels(df_var_ref_levels_sample$year_quarter)
# levels(df_fixed_threshold$year_quarter)

df_compare_predictions <- df_var_ref_levels |> 
  mutate(pred = predict(mod_run_8, newdata = df_var_ref_levels, type ="response")) |> 
  mutate(pred_cf = predict(mod_run_8, newdata = df_fixed_threshold, type ="response")) 
  # mutate(pred_cf = predict(mod_run_8, newdata = df_fixed_threshold, type ="response")) 
           
df_compare_predictions |> 
  # select(year_quarter, pred, pred_cf)
  # mutate(across(starts_with("pred"), ~ if_else(. >= .5, 1, 0))) |> 
  group_by(year_quarter) |> 
  summarise(
    att = n(), 
    actual_adm = sum(as.integer(as.character(admitted))),
    pred_adm = sum(pred),
    pred_adm_cf = sum(pred_cf),
    diff = sum(pred) - sum(pred_cf)
    ) |> 
  # print(n=50)
  saveRDS(here("data", "260126_dfq_compare_numbers_adm.rds"))



# COEFFS ------------------------------------------------------------------

df_coeff_II <- mod_run_8 |>
  broom::tidy(parametric = TRUE) |>
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96 * std.error)) |>
  mutate(uci = exp(estimate + 1.96 * std.error)) |>
  select(term, odds, lci, uci)

df_coeff_II |> saveRDS(here("data", "251208_dfq_coeff_II.rds"))
df_coeff_II |> saveRDS(here("data", "2512xx_dfq_coeff_II.rds"))
df_coeff_II |> saveRDS(here("data", "251211_dfq_coeff_II.rds"))
df_coeff_II |> saveRDS(here("data", "251212_dfq_coeff_II.rds"))

# DIAGNOSES ---------------------------------------------------------------

df_coeff_II |>
  filter(str_detect(term, "diag")) |>
  # filter(str_detect(term, "walk"))
  arrange(desc(odds)) |>
  slice_sample(n = 30) |>
  ggplot() +
  geom_hline(yintercept = 1, lty = "dashed", colour = "grey20", alpha = 0.5) +
  geom_pointrange(aes(reorder(term, odds), odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA) +
  theme_minimal() +
  coord_flip() +
  scale_y_log10() +
  theme(
     axis.title.x = element_text(size = 9),
    axis.title.y = element_blank(),
  ) +
  labs(
    y = "\nCase-mix-adjusted odds of admission (log scale)"
  )

# QUARTER ---------------------------------------------------------------

df_coeff_II |>
  filter(str_detect(term, "year")) |>
  mutate(term = str_remove(term, "year_quarter")) |> 
  mutate(row = row_number()) |> 
  ggplot() +
  geom_hline(yintercept = 1, lty = "dashed", colour = "grey20", alpha = 0.5) +
  geom_pointrange(aes(term, odds, ymin = lci, ymax = uci), col = "grey40", stroke = NA) +
  geom_smooth(aes(row, odds), se = F)+
  theme_minimal() +
  # coord_flip() +
  scale_y_log10(
    # limits = c(0.85, 1.15),
    # breaks = c(0.9, 1, 1.1),
    # labels = c(
    #   # "Half\nas likely in\n2023/24",
    #   # "Equally\nas likely in\n2023/24",
    #   # "2x\nas likely in\n2023/24",
    #   "90%\nas likely as\nav. occupancy",
    #   "Equally\nas likely as\nav. occupancy",
    #   "110%x\nas likely as\nav. occupancy"
    # )
  ) +
  scale_x_discrete(
    # breaks = seq(1, 23, 2)
    # labels = 
  )+
  theme(
    # axis.title.y = element_blank(),
    axis.text = element_text(size = 6),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_blank(),
    # plot.margin = margin(5, 20, 5, 5),
    # panel.grid.minor = element_blank(),
  ) +
  labs(
    x = "\nCase-mix-adjusted odds of admission (log scale)"
  ) 


df_var_ref_levels |> count(ethnic_grp_sus)
