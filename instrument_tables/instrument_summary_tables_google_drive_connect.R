# PURPOSE: Summarize the count and models of instruments reporting data to vlprddb by quarter from Q3 2021.
# AUTHOR: M.kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES: This version of the code downloads data from Google Drive. 

# LOCALS & SETUP ============================================================================

# Libraries
library(tidyverse)
library(googledrive)
library(gt)
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

# create data frame to make tables 
df_instruments <- vldata$`Data/instrument_master_.csv` %>% 
  left_join(vldata$`Data/site_.csv`, by = "supplier_site_id") %>%
  right_join(vldata$`Data/test_.csv`, by = "instrument_id") %>%
  left_join(vldata$`Data/country_.csv`, by = "country_code") %>%
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





