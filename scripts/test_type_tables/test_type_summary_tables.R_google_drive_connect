# PURPOSE: Summaries on number of patient samples processed and sample processing errors 
# in the last year by test_type and country.
# AUTHOR: M.kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES: This version of the code downloads data from Google Drive. 

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(googledrive)
library(gt)
library(glue)
library(zoo)
library(purrr)
library(lubridate)


# LOAD DATA ============================================================================  

  # Authorization to download data from Google Drive.  
  # request authorization from Google. If you are connecting to Google drive form R using the googledrive package for the first time
  # you can find instruction on how to gain Authorization here https://googledrive.tidyverse.org/.  
  
  drive_auth()

  # identify the csv files in the folder that contains the vlprddb tables

  folder <- drive_get(as_id("1PHP3nJTCGYMHjjdjHesHSSUzGrCrfZ45"))
  csv_files <- drive_ls(folder, type = "csv")

  # download them
  path <- paste0(getwd(),"/Data")
  csv_files %>% 
    split(csv_files$id) %>% 
    walk(~drive_download(.$id, path = file.path(path, .$name), overwrite = TRUE))

  # files downloaded as csv files from vlprddb have a date stamp appended.  We'll want to strip this out before loading data
  # otherwise we'll need to change the names of the tables in our code.
  
  folder = "Data"
  files <- list.files(folder,pattern = "*.csv",full.names = T) 
  sapply(files,FUN=function(eachPath){ 
    file.rename(from=eachPath,to= gsub('[0-9]+', '',eachPath))
  })
  
  # load all our files
  
  csv_files <- fs::dir_ls("Data/", regexp = "\\.csv$")
  vldata <- purrr::map(csv_files, read_csv)

# MUNGE ============================================================================

# create data frame to make tables of the total number of tests by test type.
  df_tests <- vldata$`Data/instrument_master_.csv` %>% 
    left_join(vldata$`Data/site_.csv`, by = "supplier_site_id") %>%
    right_join(vldata$`Data/test_.csv`, by = "instrument_id") %>%
    left_join(vldata$`Data/test_type_.csv`, by = "test_type_id") %>%
    left_join(vldata$`Data/country_.csv`, by = "country_code") %>%
    filter(test_date > Sys.Date() - years(1)) %>%
    select(instrument_id, 
           supplier_site_id,
           country_name,
           test_id,
           test_type_id,
           test_type,
           test_date,
           tot_patient_test,
           tot_patient_error) 

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
      columns = colnames(test_patient_table)
    ) %>%
    fmt_number(
      columns = colnames(test_patient_table),
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
