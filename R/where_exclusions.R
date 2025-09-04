# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_where <- here("sql", "[UDAL]where_exclusion_counts.sql")

query_where <- readChar(sql_script_where, file.info(sql_script_where)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_where_exclusions_raw <- dbGetQuery(con_test, query_where) |>
  as_tibble() |>
  clean_names()

# TODO PROBABLY NEED TO SET AN END DATE FOR ATTENDANCES

gc()
gc()
gc()

df_where_exclusions <- df_where_exclusions_raw |> 
  rename(
    procode = der_provider_code,
    # `streamed/left/died` = disstat_incomplete
    ) |> 
  arrange(procode, desc(n)) |> 
  group_by(procode) |> 
  mutate(p = n/sum(n)) |> 
  ungroup() 

df_where_exclusions |> 
  filter(procode == "RC9") |> 
  # print(n=40)
  identity()

df_where_exclusions |> saveRDS(here("data", "where_exclusion_matrix.rds"))
df_where_exclusions <- readRDS(here("data", "where_exclusion_matrix.rds"))

df_where_exclusions |> 
  filter(procode == "RC9") |> 
  # print(n=40)
  identity() |> 
  select(-p) |> 
  mutate(sigma = sum(n)) |>
  rowwise() |> 
  mutate(flag_sum = sum(c_across(c(everything(), -c(procode, n, sigma))))) |> 
  ungroup() |> 
  # REMOVE THE ROW CORRESPONDING TO ATTENDANCES FOLLOWING ALL EXCLUSIONS:
  filter(flag_sum != 0) |> 
  mutate(n_after_excl = sigma - cumsum(n) , .before = flag_sum) |> 
  mutate(n_before_excl = n_after_excl + n, .before = n) |> 
  select(-sigma) |> 
  rename(n_excl = n) |> 
  # print(n=40)
  view("large")




# ALL PROVIDERS:


# THEN DATA QUALITY