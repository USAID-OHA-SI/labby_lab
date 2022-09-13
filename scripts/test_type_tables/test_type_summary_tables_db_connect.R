# PURPOSE: Summaries on number of patient samples processed and sample processing errors 
# in the last year by test_type and country.
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
library(glue)
library(zoo)
library(lubridate)


# LOAD DATA ============================================================================  

# credentials to log into the database.  
dsn_database = 'database_name'   # Specify the name of your database.
dsn_hostname = 'database_host_name'  
dsn_port = 'port_number'                # Specify your port number as a character. e.g. 5432
dsn_uid = 'user_id'   # Specify your username. e.g. "admin"
dsn_pwd = 'passcode'        # Specify your password. e.g. "xxx"

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
test_type <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.test_type")
test_type
country <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.country")

# MUNGE ============================================================================

# create data frame to make tables of the total number of tests by test type.
df_tests <- instrument_master %>% 
  left_join(site, by = "supplier_site_id") %>%
  right_join(test, by = "instrument_id") %>%
  left_join(test_type, by = "test_type_id") %>%
  left_join(country, by = "country_code") %>%
  filter(test_date > Sys.Date() - years(1)) %>%
  select(instrument_id, 
         supplier_site_id,
         country_name,
         test_id,
         test_type_id,
         test_type,
         test_date,
         tot_patient_test,
         tot_patient_error) %>% 
  mutate_all(list(~na_if(.,""))) # need to add this because the data from the DB 
                                  # encodes all empty cells as an empty character string

# create data frame to add number of sites and instruments per country to test type tables
df_sites_inst <- df_tests %>% 
  group_by(country_name) %>%
  select(country_name, supplier_site_id, instrument_id) %>% 
  mutate(`Total sites` = length(unique(supplier_site_id))) %>%
  mutate(`Total instruments` = length(unique(instrument_id))) %>%
  distinct(country_name, .keep_all = TRUE) %>% 
  select(country_name, `Total sites`, `Total instruments`)


# group and pivot data frame to create table of total number of patient tests 
# by test type aggregated by country 

test_sample_table <- df_tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_test) %>% 
  mutate(total_patient_tests = sum(tot_patient_test)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_test) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_patient_tests
  ) %>% 
  left_join(df_sites_inst, by = "country_name") %>% 
  rename(Country = country_name)  


# group and pivot data frame to create table of total number of sample 
# processing errors by test type aggregated by country 

test_error_table <- df_tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_error) %>% 
  mutate(total_patient_errors = sum(tot_patient_error)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_error) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_patient_errors
  ) %>% 
  left_join(df_sites_inst, by = "country_name") %>% 
  rename(Country = country_name)  

# group and pivot data frame to create table of total number of sample 
# processing errors by test type aggregated by country 

test_perc_table <- df_tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_error, tot_patient_test) %>% 
  mutate(total_percent_errors = sum(tot_patient_error)/sum(tot_patient_test)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_error, -tot_patient_test) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_percent_errors
  ) %>% 
  left_join(df_sites_inst, by = "country_name") %>% 
  rename(Country = country_name)  


# VIZ ============================================================================

# save PDF of number of samples table 
test_sample_table %>%   
  gt() %>%
  tab_header(
    title = "Total number of samples run by test type by country.",
    subtitle = glue("{min(df_tests$test_date)} to {max(df_tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_sample_table)
  ) %>%
  fmt_number(
    columns = colnames(test_sample_table),
    suffixing = TRUE
  ) %>% 
  gtsave("Images/test_sample_table.pdf", expand = 15)


# save PDF of number of errors table 
test_error_table %>%   
  gt() %>%
  tab_header(
    title = "Total number of sample processing errors by test type by country.",
    subtitle = glue("{min(df_tests$test_date)} to {max(df_tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_error_table)
  ) %>%
  fmt_number(
    columns = colnames(test_error_table),
    suffixing = TRUE
  ) %>% 
  gtsave("Images/test_errors_table.pdf", expand = 15)

# save PDF of number of percent errors table 
test_perc_table %>%   
  gt() %>%
  tab_header(
    title = "Percentage of sample processing errors by test type by country.",
    subtitle = glue("{min(df_tests$test_date)} to {max(df_tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_perc_table)
  ) %>%
  fmt_percent(
    columns = colnames(test_perc_table)[1:17],
    decimals = 1
  ) %>% 
  gtsave("Images/test_perc_error_table.pdf", expand = 15)





