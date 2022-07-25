# PURPOSE: Summarize the count and models of instruments reporting data to vlprddb by quarter from Q3 2021.
# AUTHOR: M.kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES: This version of the code connects directly to vlprddb. 

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(DBI)
library(RPostgres)
library(gt)
library(zoo)
library(lubridate)


# LOAD DATA ============================================================================  

# credentials to log into the database.  
dsn_database = 'vlprddb'   # Specify the name of your database.
dsn_hostname = 'vl-prd-db.postgres.database.azure.com'  
dsn_port = '5432'                # Specify your port number as a character. e.g. 5432
dsn_uid = 'mkalnoky@vl-prd-db'   # Specify your username. e.g. "admin"
dsn_pwd = 'mka@pg17May'        # Specify your password. e.g. "xxx"

# establish a connection to the database.
connec <- DBI::dbConnect(RPostgres::Postgres(), 
                         dbname = dsn_database,
                         host = dsn_hostname, 
                         port = dsn_port,
                         user = dsn_uid, 
                         password = dsn_pwd)

# download tables from vldata schema of vlprddb database.
instrument_master <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.instrument_master")
site <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.site")
test <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.test")
country <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.country")

# MUNGE ============================================================================

# create data frame to make tables 
df_instruments <- instrument_master %>% 
  left_join(site, by = "supplier_site_id") %>%
  right_join(test, by = "instrument_id") %>%
  left_join(country, by = "country_code") %>%
  filter(test_date > "2021-07-01") %>%
  mutate(quarter_year = as.yearqtr(floor_date(test_date,unit = "quarter"))) %>% 
  mutate(instrument_model = iconv(instrument_model, "latin1", "ASCII", sub="")) %>%  # transformations to help standardize instrument names 
  mutate(instrument_model = gsub(","," ", instrument_model)) %>% # transformations to help standardize instrument names 
  mutate(instrument_model = str_squish(instrument_model)) %>% # transformations to help standardize instrument names 
  mutate(instrument_model = str_to_lower(instrument_model)) %>% # transformations to help standardize instrument names 
  select(instrument_id, 
         instrument_model, 
         supplier_name,
         supplier_site_id, 
         supplier_site_name,
         supplier_id.x, 
         country_name,
         test_type_id,
         quarter_year) 

# group and pivot to create table aggregated globally
inst_glob_table <- df_instruments %>% 
  group_by(quarter_year, instrument_model) %>%
  distinct(instrument_id) %>%
  summarise(number_instruments = length(instrument_model)) %>% 
  pivot_wider( 
    names_from = quarter_year, 
    values_from = number_instruments
  ) 


# group and pivot dataframe to create table aggregated by country 
inst_coun_table <- df_instruments %>% 
  group_by(country_name, quarter_year, instrument_model) %>%
  distinct(instrument_id) %>%
  summarise(number_instruments = length(instrument_model)) %>% 
  pivot_wider(
    names_from = quarter_year, 
    values_from = number_instruments
  ) 

# VIZ ============================================================================

# create pdf of table aggregated globally
 inst_glob_table %>%
   gt() %>%
    tab_header(
      title = "Instrument models reporting data by quarter.",
      subtitle = "Globally") %>%
    sub_missing(
      columns = colnames(inst_glob_table)
    ) %>% 
    gtsave("Images/instruments_global.pdf", expand = 15)

# create pdf of table aggregated by country
inst_coun_table %>%
  gt() %>%
  tab_header(
    title = "Instrument models reporting data by quarter.",
    subtitle = "By Country") %>%
  sub_missing(
    columns = colnames(inst_coun_table)
  ) %>% 
  gtsave("Images/instruments_country.pdf", expand = 15)





