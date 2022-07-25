library(tidyverse)
library(lubridate)
library(gt)
library(glue)
library(zoo)


csv_files <- fs::dir_ls("data/", regexp = "\\.csv$")
vldata <- purrr::map(csv_files, read_csv)

# A table of available instrument models over time by month aggregated 
# Globally
# By country
instruments <- vldata$`data/instrument_master_202206141019.csv` %>% 
                left_join(vldata$`data/site_202206141022.csv`, by = "supplier_site_id") %>%
                right_join(vldata$`data/test_202206141022.csv`, by = "instrument_id") %>%
                left_join(vldata$`data/instrument_models.csv`, by = "instrument_model") %>%
                left_join(vldata$`data/country_202206141017.csv`, by = "country_code") %>%
                filter(test_date > "2021-07-01") %>%
                rename(Instrument = standard_instrument_model) %>% 
                mutate(quarter_year = as.yearqtr(floor_date(test_date,unit = "quarter"))) %>% 
                select(instrument_id, 
                       Instrument, 
                       supplier_name,
                       supplier_site_id, 
                       supplier_site_name,
                       supplier_id.x, 
                       country_name,
                       test_type_id,
                       quarter_year) 

# global
inst_glob_table <- instruments %>% 
          group_by(quarter_year, Instrument) %>%
          distinct(instrument_id) %>%
          summarise(number_instruments = length(Instrument)) %>% 
          pivot_wider( 
            names_from = quarter_year, 
            values_from = number_instruments
            ) %>%
          gt() %>%
            tab_header(
              title = "Instrument models reporting data by quarter.",
              subtitle = "Globally") %>%
            sub_missing(
              columns = c(`2021 Q3`, `2021 Q4`,`2022 Q1`,`2022 Q2`)
            ) %>% 
          gtsave("instruments_global.pdf", expand = 15)
            

# country 
inst_coun_table <- instruments %>% 
          group_by(country_name, quarter_year, Instrument) %>%
          distinct(instrument_id) %>%
          summarise(number_instruments = length(Instrument)) %>% 
          pivot_wider(
                      names_from = quarter_year, 
                      values_from = number_instruments
                      ) %>%
          gt() %>%
          tab_header(
            title = "Instrument models reporting data by quarter.",
            subtitle = "By Country") %>%
          sub_missing(
            columns = c(`2021 Q3`, `2021 Q4`,`2022 Q1`,`2022 Q2`)
          ) %>% 
          gtsave("instruments_country.pdf", expand = 15)


####################################################################################
# Total tests by test_type

tests <- vldata$`data/instrument_master_202206141019.csv` %>% 
  left_join(vldata$`data/site_202206141022.csv`, by = "supplier_site_id") %>%
  right_join(vldata$`data/test_202206141022.csv`, by = "instrument_id") %>%
  left_join(vldata$`data/test_type_202206141023.csv`, by = "test_type_id") %>%
  left_join(vldata$`data/instrument_models.csv`, by = "instrument_model") %>%
  left_join(vldata$`data/country_202206141017.csv`, by = "country_code") %>%
  filter(test_date > "2021-07-01") %>%
  rename(Instrument = standard_instrument_model) %>% 
  select(instrument_id, 
         supplier_site_id,
         country_name,
         test_id,
         test_type_id,
         test_date,
         test_type,
         tot_patient_test,
         tot_patient_error)


tests_2 <- tests %>% 
  group_by(country_name) %>%
  select(country_name, supplier_site_id, instrument_id) %>% 
  mutate(`Total sites` = length(unique(supplier_site_id))) %>%
  mutate(`Total instruments` = length(unique(instrument_id))) %>%
  distinct(country_name, .keep_all = TRUE) %>% 
  select(country_name, `Total sites`, `Total instruments`)


# by plate
test_plate_table <- tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type) %>% 
  mutate(total_plates = length(test_type)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  pivot_wider(
               names_from = test_type, 
               values_from = total_plates
               ) %>% 
  left_join(tests_2, by = "country_name") %>% 
  rename(Country = country_name)  
  
  test_plate <- test_plate_table %>%   
  gt() %>%
  tab_header(
    title = "Total number of plates run by test type by country.",
    subtitle = glue("{min(tests$test_date)} to {max(tests$test_date)}")
    ) %>%
  sub_missing(
    columns = colnames(test_plate_table)
  ) %>%
    fmt_number(
      columns = colnames(test_plate_table),
      suffixing = TRUE
    ) %>% 
  gtsave("test_plate_table.pdf", expand = 15)


# total patient samples
test_patient_table <- tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_test) %>% 
  mutate(total_patient_tests = sum(tot_patient_test)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_test) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_patient_tests
  ) %>% 
  left_join(tests_2, by = "country_name") %>% 
  rename(Country = country_name)  

test_table <- test_patient_table %>%   
  gt() %>%
  tab_header(
    title = "Total number of samples run by test type by country.",
    subtitle = glue("{min(tests$test_date)} to {max(tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_patient_table)
  ) %>%
  fmt_number(
    columns = colnames(test_patient_table),
    suffixing = TRUE
  ) %>% 
  gtsave("test_patient_table.pdf", expand = 15)

# number of samples errors
test_error_table <- tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_error) %>% 
  mutate(total_patient_errors = sum(tot_patient_error)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_error) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_patient_errors
  ) %>% 
  left_join(tests_2, by = "country_name") %>% 
  rename(Country = country_name)  

error_table <- test_error_table %>%   
  gt() %>%
  tab_header(
    title = "Total number of sample processing errors by test type by country.",
    subtitle = glue("{min(tests$test_date)} to {max(tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_error_table)
  ) %>%
  fmt_number(
    columns = colnames(test_error_table),
    suffixing = TRUE
  ) %>% 
  gtsave("test_errors_table.pdf", expand = 15)

# percentage of error
test_perc_table <- tests %>% 
  group_by(country_name, test_type) %>%
  select(country_name, test_type, tot_patient_error, tot_patient_test) %>% 
  mutate(total_percent_errors = sum(tot_patient_error)/sum(tot_patient_test)) %>%
  distinct(country_name, test_type, .keep_all = TRUE) %>% 
  select(-tot_patient_error, -tot_patient_test) %>% 
  pivot_wider(
    names_from = test_type, 
    values_from = total_percent_errors
  ) %>% 
  left_join(tests_2, by = "country_name") %>% 
  rename(Country = country_name)  

perc_table <- test_perc_table %>%   
  gt() %>%
  tab_header(
    title = "Percentage of sample processing errors by test type by country.",
    subtitle = glue("{min(tests$test_date)} to {max(tests$test_date)}")
  ) %>%
  sub_missing(
    columns = colnames(test_perc_table)
  ) %>%
  fmt_percent(
    columns = colnames(test_perc_table)[1:17],
    decimals = 1
  ) %>% 
  gtsave("test_perc_table.pdf", expand = 15)





# errors #######################################################################
# top 5 error codes by country and instruments

error <- vldata$`data/instrument_master_202206141019.csv` %>% 
  left_join(vldata$`data/site_202206141022.csv`, by = "supplier_site_id") %>%
  right_join(vldata$`data/test_202206141022.csv`, by = "instrument_id") %>%
  right_join(vldata$`data/error_202206141018.csv`, by = "test_id") %>%
  left_join(vldata$`data/instrument_models.csv`, by = "instrument_model") %>%
  filter(test_date > "2021-07-01") %>%
  select(instrument_id, 
         instrument_model, 
         supplier_name,
         supplier_site_name,
         country_code, 
         test_id,
         test_type_id,
         test_date,
         error_type_id)

country <- error %>% 
  group_by(country_code, error_type_id) %>%
  distinct(error_type_id) %>%
  summarise(number_of_errors = length(error_type_id))
