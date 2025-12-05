# README
# Get model stats and results.

mod_run_1 <- readRDS(here::here("data", "251203_mod_run_1.rds"))


# STATS -------------------------------------------------------------------

tmp_stats <- tibble(
  # obs = as.factor(mod_run_1$y),
  obs = df_var_ref_levels_sample_1m$admitted,
  fit = 1 - fitted(mod_run_1) # predicted probabilities 1
  # fit = fitted(mod_run_1) # predicted probabilities 1
)

tibble(data = list)

tmp_stats |> brier_class(obs, fit)
tmp_stats |> brier_class(obs1, fit)
tmp_stats |> roc_auc(obs, fit)
# tmp_stats |> roc_auc(obs, fit, event_level = "second")
# tmp_stats |> roc_auc(obs1, fit, event_level = "second")


# COEFFS ------------------------------------------------------------------

df_coeff_II <- mod_run_1 |>
  broom::tidy(parametric = TRUE) |>
  mutate(odds = exp(estimate)) |>
  mutate(lci = exp(estimate - 1.96 * std.error)) |>
  mutate(uci = exp(estimate + 1.96 * std.error)) |>
  select(term, odds, lci, uci)

df_coeff_II |> saveRDS(here("data", "251204_dfq_coeff_II.rds"))

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
