#### Title
# PURPOSE: Roche Plate Errors Take 2
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-12-20
# NOTES: 

#### LOCALS & SETUP ============================================================================

#### TOLEDO: Make sure there are no covid tests included here!


library(tidyverse)
library(googledrive)
library(gagglr)
require(here)
require(lubridate)
require(scales)

# Set paths  
data   <- here("Data")
dataout <- here("Dataout")
  

#### LOAD DATA ============================================================================  


drive_auth()

folder <- "198YgVgnr9_z6YhqbQWbDlo0GJK70u_cV"

files_in_folder <- googledrive::drive_ls(googledrive::as_id(folder))

glamr::import_drivefile(drive_folder = folder,
                        filename = "test_activity_roche.csv",
                        folderpath = data,
                        zip = FALSE)

glamr::import_drivefile(drive_folder = folder,
                        filename = "test_rate.csv",
                        folderpath = data,
                        zip = FALSE)

glamr::import_drivefile(drive_folder = folder,
                        filename = "test.csv",
                        folderpath = data,
                        zip = FALSE)

glamr::import_drivefile(drive_folder = folder,
                        filename = "instrument_master.csv",
                        folderpath = data,
                        zip = FALSE)
glamr::import_drivefile(drive_folder = folder,
                        filename = "error.csv",
                        folderpath = data,
                        zip = FALSE)

glamr::import_drivefile(drive_folder = folder,
                        filename = "error_type.csv",
                        folderpath = data,
                        zip = FALSE)
glamr::import_drivefile(drive_folder = folder,
                        filename = "site.csv",
                        folderpath = data,
                        zip = FALSE)
glamr::import_drivefile(drive_folder = folder,
                        filename = "test_type.csv",
                        folderpath = data,
                        zip = FALSE)


test_activity_roche <- read_csv(here("Data", "test_activity_roche.csv"))

test_rate <- read_csv(here("Data", "test_rate.csv"))

test = read_csv(here("Data", "test.csv"))

instrument_master = read_csv(here("Data", "instrument_master.csv"))

error = read_csv(here("Data", "error.csv"))

error_type = read_csv(here("Data", "error_type.csv"))

site = read_csv(here("Data", "site.csv"))

test_type = read_csv(here("Data", "test_type.csv"))


#### DATA WRANGLING ============================================================================

test_type = test_type %>%
  select(test_type_id, test_type_psm)

instrument_master = instrument_master %>%
  select(instrument_id, instrument_model, supplier_site_id) %>%
  mutate(instrument_id = as.numeric(instrument_id)) %>%
  filter(instrument_id %in% unique(test_activity_roche$instrument_id)) %>%
  mutate(instrument_model = toupper(instrument_model))

test_activity_roche = test_activity_roche %>%
  left_join(instrument_master, by = c("instrument_id" = "instrument_id"))

test_activity_roche = test_activity_roche %>%
  filter(str_detect(instrument_model, "00")) %>%
  mutate(instrument_model = str_remove_all(instrument_model, "®"))


error_codes = test_activity_roche %>%
  mutate(internal_id = paste0("INTERNAL-",row_number())) %>%
  separate(error_code_list, c("a","b","c","d","e","f","g","h", "i"), sep = ",") %>%
  pivot_longer(cols = c("a","b","c","d","e","f","g","h", "i"), values_to = "error_code_list") 

error_codes = error_codes %>%
  filter(name == "a" | !is.na(error_code_list))


error_type = error_type %>%
  filter(supplier_id == 1000 &
           error_type_id %in% error_codes$error_code_list) %>%
  select(error_type_id, error_type_description)

site = site %>%
  select(supplier_site_id, supplier_site_name)

roche_errors = error_codes %>% 
  left_join(error_type, by = c("error_code_list" = "error_type_id")) %>%
  left_join(site, by = c("supplier_site_id" = "supplier_site_id")) %>%
  select(internal_id,
         name, 
         error_code_list,
         error_type_description,
         instrument_id,
         instrument_model,
         supplier_site_id,
         supplier_site_name,
         country_code,
         test_type_id,
         test_date,
         is_patient_test,
         is_control_test,
         patient_test_error_flag,
         control_test_error_flag,
         excluded_test) %>%
  mutate(month = floor_date(test_date, unit = "month")) %>%
  mutate(test_month_year = zoo::as.yearmon(month)) %>%
  select(-month) %>%
  mutate(id_error = paste0(internal_id, "-", error_code_list)) %>%
  mutate(dups = duplicated(id_error))

# Filter out the duplicate errors

roche_errors = roche_errors %>%
  filter(dups == F) %>%
  select(-id_error, -dups)

roche_errors$error_type_description[roche_errors$error_code_list=="6282.36"]<-"Pipetted volume was not sufficient. Sample was not transferred"

write_csv(roche_errors, here(dataout, "roche_errors.csv"))
#roche_errors = read_csv(here(dataout, "roche_errors.csv"))


roche_errors %>% 
  group_by(error_code_list, error_type_description) %>%
  filter(!is.na(error_code_list)) %>%
  summarize(n = n()) %>%
  mutate(error_percent = n*100/1094225) %>%
  arrange(desc(error_percent)) %>%
  write_csv(here("Dataout", "roche_tables", "all_error_percentages.csv"))


# Errors by supplier
supplier_errors = roche_errors %>%
  filter(!duplicated(internal_id)) %>%
  group_by(supplier_site_name) %>%
  summarize(tests_run = n())

roche_errors %>%
  filter(!is.na(error_code_list)) %>%
  group_by(country_code, supplier_site_name, error_code_list, error_type_description) %>%
  summarize(n = n()) %>%
  left_join(supplier_errors, by = c("supplier_site_name" = "supplier_site_name")) %>%
  mutate(error_percent = n*100/tests_run) %>%
  arrange(supplier_site_name, desc(error_percent)) %>%
  write_csv(here("Dataout", "roche_tables", "roche_errors_site.csv"))

# Errors by Instrument
instrument_errors = roche_errors %>%
  filter(!duplicated(internal_id)) %>%
  group_by(instrument_id) %>%
  summarize(tests_run = n())

roche_errors %>%
  filter(!is.na(error_code_list)) %>%
  group_by(instrument_id, instrument_model, country_code, supplier_site_name, error_code_list, error_type_description) %>%
  summarize(n = n()) %>%
  left_join(instrument_errors, by = c("instrument_id" = "instrument_id")) %>%
  mutate(error_percent = n*100/tests_run) %>%
  arrange(supplier_site_name, desc(error_percent)) %>%
  write_csv(here("Dataout", "roche_tables", "roche_errors_instrument.csv"))


# Errors by Month
month_errors = roche_errors %>%
  filter(!duplicated(internal_id)) %>%
  group_by(country_code, test_month_year) %>%
  summarize(tests_run = n())

roche_errors %>%
  filter(!is.na(error_code_list)) %>%
  group_by(country_code, test_month_year, error_code_list, error_type_description) %>%
  summarize(n = n()) %>%
  left_join(month_errors) %>%
  mutate(error_percent = n*100/tests_run) %>%
  arrange(test_month_year, desc(error_percent)) %>%
  view()
  write_csv(here("Dataout", "roche_tables", "roche_errors_month.csv"))
