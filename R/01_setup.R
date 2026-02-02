# README
# Load packages and establish DB connection. 
# Note: you will be prompted for a password via pop-up.

library("gt")
library("gtExtras")
library("DBI")
# library("pak")
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
library("keyring")
library("stringr")
library("tsibble")
library("lubridate")
library("patchwork")
library("yardstick")

options(scipen=999) 

server <- keyring::key_get("server")
db <- keyring::key_get("db")

con_test <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = server,
  Database = db,
  Authentication = "ActiveDirectoryInteractive"
)

# REMEMBER POP-UP!
