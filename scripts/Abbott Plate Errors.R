#### Title
# PURPOSE: Abbott Plate Errors
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-12-20
# NOTES: 

#### LOCALS & SETUP ============================================================================


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
                        filename = "test_activity_abbott.csv",
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


test_activity_abbott <- read_csv(here("Data", "test_activity_abbott.csv"))

test_rate <- read_csv(here("Data", "test_rate.csv"))

test = read_csv(here("Data", "test.csv"))

instrument_master = read_csv(here("Data", "instrument_master.csv"))

error = read_csv(here("Data", "error.csv"))

error_type = read_csv(here("Data", "error_type.csv"))

site = read_csv(here("Data", "site.csv"))

test_type = read_csv(here("Data", "test_type.csv"))


#### DATA WRANGLING ============================================================================

# Cleaning up input datasets

instrument_master = instrument_master %>%
  select(instrument_id, instrument_model, supplier_site_id) %>%
  filter(instrument_id %in% unique(test_activity_abbott$instrument_id)) %>%
  mutate(instrument_model = toupper(instrument_model))

test_activity_abbott = test_activity_abbott %>%
  left_join(instrument_master, by = c("instrument_id" = "instrument_id"))

test_activity_abbott = test_activity_abbott %>%
  mutate(instrument_model = str_remove_all(instrument_model, "®"))

error_type = error_type %>%
  select(error_type_id, error_type_name, error_type_description)

site = site %>%
  select(supplier_site_id, supplier_site_name)

test_type = test_type %>%
  select(test_type_id, test_type_psm)

# Obtaining raw error codes data

error_codes = test_activity_abbott %>%
  left_join(test_type) %>%
  filter(test_type_psm %in% c("EID", "EID/VL", "VL")) %>%
  mutate(well_number = str_extract(test_record_id, "\\|\\d+$")) %>%
  mutate(well_number = str_remove_all(well_number, "\\|")) %>%
  mutate(test_record_id = str_remove(test_record_id, "\\|\\d+$")) %>%
  filter(!is.na(error_type_id)) %>%
  mutate(error_type_id = as.character(error_type_id)) %>%
  left_join(error_type, by = c("error_type_id" = "error_type_id")) %>%
  left_join(site, by = c("supplier_site_id" = "supplier_site_id")) %>%
  select(instrument_id,
         instrument_model,
         supplier_site_id,
         supplier_site_name,
         country_code,
         well_number,
         test_type_id,
         test_type_psm,
         error_type_id,
         error_type_name,
         error_type_description,
         error_count,
         test_date,
         is_patient_test,
         is_control_test,
         patient_test_error_flag,
         control_test_error_flag,
         excluded_tests
         ) %>%
  mutate(test_date = as.character(test_date)) %>%
  mutate(test_month_year = format(ymd(test_date), "%m-%Y"))
  
write_csv(error_codes, here("Dataout", "error_codes_abbott.csv"))

# Error Codes as a percentage of plates affected

## Cleaning up test_activity_abbott

tas_clean = test_activity_abbott %>%
  left_join(test_type) %>%
  filter(test_type_psm %in% c("EID", "EID/VL", "VL")) %>%
  mutate(test_date = as.character(test_date)) %>%
  mutate(test_month_year = format(ymd(test_date), "%m-%Y")) %>%
  mutate(well_number = str_extract(test_record_id, "\\|\\d+$")) %>%
  mutate(well_number = str_remove_all(well_number, "\\|")) %>%
  mutate(test_record_id = str_remove(test_record_id, "\\|\\d+$")) %>%
  mutate(error_type_id = as.character(error_type_id)) %>%
  left_join(error_type, by = c("error_type_id" = "error_type_id")) %>%
  left_join(site, by = c("supplier_site_id" = "supplier_site_id"))

## Obtain number of plates affected by each code with several disaggregates

affected_plates = tas_clean  %>%
  filter(error_count>0) %>%
  group_by(test_record_id,
           test_month_year,
           instrument_model,
           instrument_id,
           supplier_site_name,
           country_code,
           error_type_id) %>%
  summarize(errors = n()) %>% 
  ungroup() %>%
  group_by(test_month_year, 
           error_type_id,
           instrument_model,
           instrument_id,
           supplier_site_name,
           country_code
           ) %>%
  summarize(plates_affected = n())

### Summing down to only month-year

affected_plates_my = affected_plates %>%
  ungroup() %>%
  group_by(test_month_year, error_type_id) %>%
  summarize(plates_affected = sum(plates_affected, na.rm = T))

### Summing down to month-year and country

affected_plates_my_country = affected_plates %>%
  ungroup() %>%
  group_by(test_month_year, country_code, error_type_id) %>%
  summarize(plates_affected = sum(plates_affected, na.rm = T))

### Summing down to month-year, country, and site

affected_plates_my_country_site = affected_plates %>%
  ungroup() %>%
  group_by(test_month_year, country_code, supplier_site_name, error_type_id) %>%
  summarize(plates_affected = sum(plates_affected, na.rm = T))


## Obtain total number of plates run with several disaggregates


total_plates = tas_clean %>%
  group_by(test_month_year, 
           country_code, 
           supplier_site_name, 
           instrument_model, 
           instrument_id, 
           test_record_id) %>%
  summarize(wells_per_plate = n())  %>%
  ungroup() %>%
  group_by(test_month_year, 
           country_code, 
           supplier_site_name, 
           instrument_model, 
           instrument_id
           ) %>%
  summarize(plates = n())

### Summing down to only month-year

total_plates_my = total_plates %>%
  ungroup() %>% 
  group_by(test_month_year) %>%
  summarize(plates = sum(plates, na.rm = T))

### Summing down to month-year and country

total_plates_my_country = total_plates %>%
  ungroup() %>% 
  group_by(test_month_year, country_code) %>%
  summarize(plates = sum(plates, na.rm = T))

### Summing down to month-year, country, and site

total_plates_my_country_site = total_plates %>%
  ungroup() %>% 
  group_by(test_month_year, country_code, supplier_site_name) %>%
  summarize(plates = sum(plates, na.rm = T))


## Joining disaggs together

percent_plates_my = affected_plates_my %>%
  left_join(total_plates_my) %>%
  mutate(percent = plates_affected*100/plates) %>%
  write_csv(here("Dataout", "percent_plates_my.csv"))

percent_plates_my_country = affected_plates_my_country %>%
  left_join(total_plates_my_country) %>%
  mutate(percent = plates_affected*100/plates) %>%
  write_csv(here("Dataout", "percent_plates_my_country.csv"))

percent_plates_my_country_site = affected_plates_my_country_site %>%
  left_join(total_plates_my_country_site) %>%
  mutate(percent = plates_affected*100/plates) %>%
  write_csv(here("Dataout", "percent_plates_my_country_site.csv"))
