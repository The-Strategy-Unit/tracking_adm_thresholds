
# 3. ECDS -------------------------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_ecds_II <- here("sql", "[UDAL]phase_II_query_ecds.sql")

query_ecds_II <- readChar(sql_script_ecds_II, file.info(sql_script_ecds_II)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_ecds_II_raw <- dbGetQuery(con_test, query_ecds_II) |>
  as_tibble() |>
  clean_names()

gc()
gc()
gc()
gc()

# 2. REFERENCE DATA FOR SNOMED CODES -------------------------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_reference <- here("sql", "[UDAL]query_ecds_reference.sql")

query_reference <- readChar(sql_script_reference, file.info(sql_script_reference)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

df_reference_raw <- dbGetQuery(con_test, query_reference) |>
  as_tibble() |>
  clean_names()

gc()
gc()

df_ref <- df_reference_raw |>
  mutate(sheet_name = str_to_lower(sheet_name)) |>
  # DESIRED FIELDS ONLY:
  filter(
    str_detect(
      sheet_name,
      "mode|acuity|accom|spoken|attend|discharge status|dest|complaint"
    )
  ) |>
  separate(col = sheet_name, into = c("sheet_no", "var"), sep = "^\\S*\\K\\s+") |>
  arrange(desc(created_date)) |>
  group_by(var, snomed_code) |>
  # TAKE MOST RECENT LOOKUPS:
  filter(row_number() == 1) |>
  ungroup() |>
  select(-c(sheet_no, created_date)) |>
  select(-var) 

###

df_ref_trimmed <- df_ref |> 
  select(snomed_code, derived_snomed_descr)

df_ref_diagnosis <- df_reference_raw |>
  mutate(sheet_name = str_to_lower(sheet_name)) |>
  filter(str_detect(sheet_name, "diagnosis$")) |> 
  arrange(desc(created_date)) |>
  group_by(snomed_code) |>
  # TAKE MOST RECENT LOOKUPS:
  filter(row_number() == 1) |>
  ungroup() |>
  select(-created_date) |>
  rename(diag_descr_snomed = derived_snomed_descr)


# # 3. ADDTIONAL TABLE FOR SNOMED TO ICD-10 DIAGNOSIS CONVERSION ----------
# 
# # READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
# sql_script_diag_convert <- here("sql", "[UDAL]phase_II_query_diag_convert.sql")
# 
# query_diag_convert <- readChar(sql_script_diag_convert, file.info(sql_script_diag_convert)$size) |>
#   str_replace_all(string = _, "\n|\r|ï»¿", " ")
# 
# 
# lkp_diag_icd10 <- dbGetQuery(con_test, query_diag_convert) |>
#   as_tibble() |>
#   clean_names() 
# 
# gc()
# gc()
# gc()
# 

# 4. OTHER REFERENCE TABLES -------------------------------------------

# READ AND RUN QUERY FROM SCRIPT IN THE SQL FOLDER:
sql_script_ethref <- here("sql", "[UDAL]query_eth_ref.sql")

query_ethref <- readChar(sql_script_ethref, file.info(sql_script_ethref)$size) |>
  str_replace_all(string = _, "\n|\r|ï»¿", " ")

lkp_ethref_raw <- dbGetQuery(con_test, query_ethref) |>
  as_tibble() |>
  clean_names() |> 
  select(-udal_file_id) |> 
  rename(ethnic_grp_sus = description)

gc()
gc()
gc()

