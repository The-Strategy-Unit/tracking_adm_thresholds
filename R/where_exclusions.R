# README
# Pulls through a table from DB via SQL query, showing number
# of attendances due to the exclusions applied in the primary
# ecds query.
# Note: has no date cut-off.


# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_where <- here("sql", "[UDAL]where_exclusion_counts.sql")

query_where <- readChar(sql_script_where, file.info(sql_script_where)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_where_exclusions_raw <- dbGetQuery(con_test, query_where) |>
  as_tibble() |>
  clean_names()


gc()
gc()
gc()

df_where_exclusions <- df_where_exclusions_raw |> 
  arrange(der_provider_code, desc(n)) |> 
  group_by(der_provider_code) |> 
  mutate(p = n/sum(n)) |> 
  ungroup() 

# df_where_exclusions |> saveRDS(here("data", "where_matrix.rds"))

param_provider_site <- "RWA01"

readRDS(here("data", "where_matrix.rds")) |> 
  filter(der_provider_site_code == param_provider_site) |> 
  mutate(sigma = sum(n)) |>
  rowwise() |> 
  mutate(row_sum = sum(c_across(c(everything(), -c(der_provider_code,der_provider_site_code, n, p, sigma))))) |> 
  ungroup() |> 
  # REMOVE THE ROW CORRESPONDING TO ATTENDANCES FOLLOWING ALL EXCLUSIONS:
  filter(row_sum != 0) |> 
  mutate(n_after_excl = sigma - cumsum(n)) |> 
  # view()
  mutate(n_before_excl = n_after_excl + n) |> 
  select(-c(sigma, p)) |> 
  rename(n_excl = n) |> 
  relocate(c(n_before_excl, n_excl, n_after_excl), .after = row_sum) |> 
  # print(n=40)
  view("where")



