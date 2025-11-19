# README
# Produce XMR charts and plots to highlight non constant risk by week.

# CUT OFF DATE IS MOST RECENT SUNDAY MINUS THREE WEEKS:
param_date_cutoff <- set_cutoff_date(min_buffer_days = 21)

param_date_start <- get_first_monday_fyear(2025)


# (WEEK BEGINNING)
lkp_term_to_date <- seq(
  param_date_start,
  param_date_cutoff,
  by = "1 week"
) |> 
  enframe(name = NULL, value = "date") |> 
  mutate(term = yearweek(date)) |> 
  mutate(term = str_c("year_week", term)) |> 
  mutate(term_int_diag = str_c(term, ":na_diag1")) |> 
  mutate(term_int_comp = str_c(term, ":na_complaint1")) 


# PLOT INTERACTION TO HIGHLIGHT NON-CONSTANT RISKS ------------------

# POSSIBLY DON'T NEED PLOTS - IF TAKEN FURTHER WILL JUST
# HIGHLIGHT ANOMALOUS WEEKS ON XMR CHARTS

# df_coeff$mod_coeffs[[2]]


df_risk_plots <- df_coeff |>
  # FOR INTERACTION MODELS:
  filter(mod_spec %in% c("a1", "b1")) |>
  # relocate(ed_name, .after = der_provider_site_code) |> 
  # arrange(dq) |> 
  mutate(row_no = row_number()) |> 
  mutate(data_risk = pmap(
    list(mod_coeffs, der_provider_site_code, mod_spec),
    function(df, x, y) {
      
      if (str_detect(y, "a")) {
        df |>
          filter(str_detect(term, ":na")) |>
          # select(term, estimate) |>
          left_join(lkp_term_to_date, join_by(term == term_int_diag)) |>
          arrange(date) 
        # mutate(across(where(is.numeric), ~ exp(.)))
        
        
      } else if (str_detect(y, "b")) {
        
        df |>
          filter(str_detect(term, ":na")) |>
          # select(term, estimate) |>
          left_join(lkp_term_to_date, join_by(term == term_int_comp)) |>
          arrange(date) 
        # mutate(across(where(is.numeric), ~ exp(.)))
        # ggplot(aes(date, estimate)) +
        # theme_minimal() +
        # geom_point() +
        # geom_line() +
        # geom_blank(aes(y = 0))
      }
    }
  )) |> 
  mutate(data_risk = pmap(
    list(data_risk),
    function(df){
      df |> 
        # ADD 80% CI AND RECALIBRATE THRESHOLDS:
        mutate(lci = exp(estimate - 1.282*std.error)) |> 
        mutate(uci = exp(estimate + 1.282*std.error)) |> 
        mutate(av = median(odds)) |> 
        mutate(upp_lim = 2*av) |>
        mutate(low_lim = 0.5*av)
        # mutate(upp_lim = 2) |> 
        # mutate(low_lim = 0.5)
    }
  )) |> 
# df_risk_plots$data_risk[[1]]
    mutate(plot_risk = pmap(
    list(data_risk, ed_name, row_no),
    function(df, x, y){
     df |>
        ggplot(aes(date, odds)) +
        theme_minimal() +
        geom_pointrange(aes(ymin = lci, ymax = uci)) +
        geom_hline(aes(yintercept = upp_lim[[1]]), lty = "dashed")+
        geom_hline(aes(yintercept = low_lim[[1]]), lty = "dashed")+
        geom_blank(aes(y = 0))+
        # TODO LOG SCALE !!
        labs(title = str_c(y, ". ", x))
      
    }
  ))

df_risk_plots$mod_coeffs[[7]] |> 
  # filter(str_detect(term, ":na")) |> 
  print(n=400)


df_risk_plots$plot_risk

# df_risk_plots$data_risk[[2]] 
  # TODO INTEGRATE THIS INFO IN CONTROL CHART
  # TODO JOIN ON TERM THAT YOU'VE MUTATED (RM INTERACTION CHARS)
  
  
#   mutate(flag_risk = if_else(
#     (uci < low_lim | lci > upp_lim) & (odds != 1), 1, 0
#   )) |> 
#   # select(term, odds, lci, uci, upp_lim, low_lim, flag_ncr) |> 
#   select(date, flag_risk)
#   # print(n= 26)
#   identity()
# 
# levels(df_risk_plots$data[[1]]$year_week)

# 
# df_risk_plots$data[[1]] |> 
#   count(year_week, na_diag) |> 
#   mutate(p = n/sum(n)) |> 
#   filter(na_diag == 1) |> 
#   print(n=40)
# 
# df_risk_plots$data[[2]] |> 
#   count(year_week, na_complaint) |> 
#   mutate(p = n/sum(n)) |> 
#   filter(na_complaint == 1) |> 
#   print(n=40)

df_flag_risk <- df_risk_plots |> 
  mutate(data_flag = pmap(
    list(data_risk),
    function(df){
      df |> 
        mutate(flag_risk = if_else(
          (uci < low_lim | lci > upp_lim) & (odds != 1), "1", "0"
        )) |> 
        select(date, flag_risk)
    }
  )) |> 
  select(der_provider_site_code, data_flag)

df_flag_risk$data_flag[[7]] |> 
  print(n=30)
# 
# df_flag_risk |> 
#   unnest(data_flag) |> 
#   filter(flag_risk == 1)
  

# -------------------------------------------------------------------------

df_prep_plots$prep_xmr[[7]]

df_coeff$mod_coeffs[[7]] 


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
      mutate(across(where(is.numeric), ~ exp(.))) |> 
      mutate(across(c(mr, mr_mean, ucl_mr), ~ log(.)))
    
  } ))

# df_flag_risk$data_flag[[1]]
# df_prep_plots$prep_xmr[[7]] |> 
#   view("estimates")


df_prep_plots$prep_xmr[[7]] |> 
  left_join(df_flag_risk$data_flag[[7]], join_by(date))


df_xmr_plots <- df_prep_plots |> 
  # DON'T NEED XMR PLOTS FOR INTERACTION MODELS:
  filter(!mod_spec %in% c("a1", "b1")) |> 
  # ADD NON CONSTANT RISK FLAG DF:
  left_join(df_flag_risk, join_by(der_provider_site_code)) |> 
  mutate(plot_x = pmap(
    list(prep_xmr, data_flag, ed_name),
    function(df1, df2, x){
    df1 |> 
      left_join(df2, join_by(date)) |> 
      ggplot(aes(date, estimate))+
      geom_hline(aes(yintercept = x_mean[1]), lty = "dashed")+
      geom_hline(aes(yintercept = ucl[1]), lty = "dashed")+
      geom_hline(aes(yintercept = lcl[1]), lty = "dashed")+
      # annotate("text", x = param_date_start , y = 1.1*max(df1$estimate), hjust = 0, label = "↑ Higher odds = Lower admission threshold ↓", size = 2)+
      geom_point(aes(colour = flag_risk)) +
      scale_color_manual(
        values = c("grey20", "indianred"),
        name = "Evidence of non-constant risks:",
        labels = c("No", "Yes")
        )+
      geom_line()+
      theme_minimal()+
      scale_y_log10()+
      theme(
        axis.title = element_text(size = 7),
        # legend.title = element_blank(),
        legend.position = "none"
        )+
      scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
      labs(
        title = str_c(x, "\n"),
        # title = str_c(y, ". ", x)
        x = "\nDate",
        y = str_c(
          "Casemix-adjusted odds of admission (log scale)\n(Reference category: w/c April 7th 2025)\n"
        )
      )
  }
  )) |> 
  mutate(plot_mr = pmap(
           list(prep_xmr, data_flag, ed_name),
         function(df1, df2, x){
           df1 |> 
             left_join(df2, join_by(date)) |> 
             # THE FIRST MR WILL BE NA AND DON'T WANT THIS ON SCALE:
             filter(!is.na(mr)) |> 
      ggplot(aes(date, mr))+
      geom_hline(aes(yintercept = mr_mean[1]), lty = "dashed")+
      geom_hline(aes(yintercept = ucl_mr[1]), lty = "dashed")+
      geom_point(aes(colour = flag_risk)) +
             scale_color_manual(
               values = c("grey20", "indianred3"),
               name = "Evidence of non-constant risks:",
               labels = c("No", "Yes")
             )+
      geom_line()+
      theme_minimal()+
      # scale_y_log10()+
      theme(
        axis.title = element_text(size = 7),
        legend.position = "bottom",
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 7)
        )+
      scale_x_date(date_breaks = "month", date_labels = "%b %Y")+
      labs(
        x = "\nDate",
        y = str_c(
          "Moving range (of odds)\n"
        )
      )
  })) |> 
  mutate(plot_both = map2(plot_x, plot_mr, function(p1, p2){
    
    p1/p2
    
  }))



df_xmr_plots$plot_both[[1]]
df_xmr_plots$plot_both[[7]]

df_xmr_plots$prep_xmr[[1]] |> 
  select(term)

df_risk_plots$plot_risk[[3]]
df_xmr_plots$plot_x[[7]]
df_xmr_plots$plot_mr[[7]]
