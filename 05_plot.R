# README
# Produce XMR charts and plots to highlight non constant risk by week.

# CUT OFF DATE IS MOST RECENT SUNDAY MINUS THREE WEEKS:
# param_date_cutoff <- set_cutoff_date(min_buffer_days = 21)

param_date_start <- get_first_monday(2025)


# (WEEK BEGINNING)
lkp_term_to_date <- seq(
  param_date_start,
  param_date_cutoff,
  by = "1 week"
) |> 
  enframe(name = NULL, value = "date") |> 
  mutate(term = yearweek(date)) |> 
  mutate(term = str_c("year_week", term))
  # mutate(term_interaction = str_c(term:na_diag1))

df_prep_plots <- df_coeff |> 
  mutate(prep_xmr = map(mod_coeffs, function(df){
    df |> 
      select(term, estimate) |> 
      filter(str_detect(term, "year_week")) |>
      # REMOVE INTERACTION TERMS, IF PRESENT:
      filter(!str_detect(term, "diag")) |>
      left_join(lkp_term_to_date, join_by(term)) |> 
      add_row(estimate = 0, date = param_date_start) |> 
      arrange(date) |> 
      ###   
      mutate(mr = abs(estimate - lag(estimate))) |> 
      mutate(x_mean = mean(estimate)) |> 
      mutate(mr_mean = mean(mr, na.rm = T)) |> 
      mutate(ucl = x_mean + (2.66*mr_mean)) |> 
      mutate(lcl = x_mean - (2.66*mr_mean)) |> 
      mutate(ucl_mr = mr_mean*3.268) |> 
      mutate(across(where(is.numeric), ~ exp(.)))
    
  } ))


df_xmr_plots <- df_prep_plots |> 
  # DON'T NEED XMR PLOTS FOR INTERACTION MODELS:
  filter(!mod_spec %in% c("a1", "b1")) |> 
  mutate(plot_x = map2(prep_xmr, der_provider_site_code, function(df, x){
    df |> 
      ggplot(aes(date, estimate))+
      geom_hline(aes(yintercept = x_mean[1]), lty = "dashed")+
      geom_hline(aes(yintercept = ucl[1]), lty = "dashed")+
      geom_hline(aes(yintercept = lcl[1]), lty = "dashed")+
      annotate("text", x = param_date_start , y = 1.1*max(df$estimate), hjust = 0, label = "↑ Higher admission threshold")+
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
          x, ")\n(Reference category: w/c Jan 6th 2025)\n"
        )
      )
  }
  )) |> 
  mutate(plot_mr = map2(prep_xmr, der_provider_site_code, function(df, x){
    df |> 
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
          x, ")\n"
        )
      )
  })) |> 
  mutate(plot_both = map2(plot_x, plot_mr, function(p1, p2){
    
    p1/p2
    
  }))


df_plots$plot_x[[1]]
df_plots$plot_mr[[1]]
df_plots$plot_both[[2]]

# PLOT INTERACTION TO HIGHLIGHT NON-CONSTANT RISKS ------------------
# POSSIBLY DON'T NEED PLOTS - IF TAKEN FURTHER WILL JUST
# HIGHLIGHT ANOMALOUS WEEKS ON XMR CHARTS

# TODO THIS IS UNFINISHED - NEED TO VARY TERM MATCHING WEEK , BETWEEN DIAG AND COMPLAINT

df_coeff$mod_coeffs[[4]] |> 
  # select(term) |> 
  # print(n=400) 
  filter(str_detect(term, ":na")) |> 
  select(term, estimate) |> 
  # filter(str_detect(term, "year_week")) |>
  # REMOVE INTERACTION TERMS, IF PRESENT:
  # filter(!str_detect(term, "diag")) |>
  left_join(lkp_term_to_date, join_by(term)) |> 
  # add_row(estimate = 0, date = param_date_start) |> 
  arrange(date) |> 
  mutate(across(where(is.numeric), ~ exp(.)))
# mutate(week = row_number()) |> 
ggplot(aes(date, estimate))+
  theme_minimal()+
  geom_point()+
  geom_line()+
  geom_blank(aes(y =0))

df_risk_plots <- df_coeff |> 
  # FOR INTERACTION MODELS:
  filter(mod_spec %in% c("a1", "b1")) |> 
  mutate(plot_risk = map2(mod_coeffs, der_provider_site_code, function(df, x){
    df |> 
      filter(str_detect(term, ":na")) |> 
      select(term, estimate) |> 
      # filter(str_detect(term, "year_week")) |>
      # REMOVE INTERACTION TERMS, IF PRESENT:
      # filter(!str_detect(term, "diag")) |>
      left_join(lkp_term_to_date, join_by(term)) |> 
      # add_row(estimate = 0, date = param_date_start) |> 
      arrange(date) |> 
      mutate(across(where(is.numeric), ~ exp(.)))
      # mutate(week = row_number()) |> 
      ggplot(aes(date, estimate))+
      theme_minimal()+
      geom_point()+
      geom_line()+
      geom_blank(aes(y =0))
  }
  ))

df_risk_plots$plot_risk[[2]]
