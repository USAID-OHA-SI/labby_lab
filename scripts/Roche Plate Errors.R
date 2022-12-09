#### Title
# PURPOSE: Cleaning and Analysis of Roche Plate Errors
# AUTHOR: alerichardson | sch
# LICENSE: MIT
# DATE: 2022-11-28
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


test_activity_roche <- read_csv(here("Data", "test_activity_roche.csv"))

test_rate <- read_csv(here("Data", "test_rate.csv"))

test = read_csv(here("Data", "test.csv"))

instrument_master = read_csv(here("Data", "instrument_master.csv"))

#### DATA WRANGLING - First Pass ============================================================================
  
glimpse(test_activity_roche)
glimpse(test_rate)
glimpse(test)

test %>%
  filter(supplier_id==1000) %>%
  group_by(test_date) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

instrument_master = instrument_master %>%
  select(instrument_id, instrument_model) %>%
  mutate(instrument_id = as.numeric(instrument_id)) %>%
  filter(instrument_id %in% unique(test_activity_roche$instrument_id)) %>%
  mutate(instrument_model = toupper(instrument_model))
  

test_activity_roche = test_activity_roche %>%
  left_join(instrument_master, by = c("instrument_id" = "instrument_id"))

test_activity_roche %>%
  group_by(country_code, instrument_model) %>%
  summarize(instruments = length(unique(instrument_id)),
            tests = n()) %>%
  write_csv(paste0(dataout, "/countries_models.csv"))

error_codes = test_activity_roche %>%
  filter(!is.na(error_code_list)) %>%
  select(country_code, instrument_id, instrument_model, error_code_list) %>%
  separate(error_code_list, c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o"), sep = ",") %>%
  pivot_longer(cols = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o"), values_to = "error_code_list") %>%
  select(-name) %>%
  filter(!is.na(error_code_list)) %>%
  mutate(instrument_model = toupper(instrument_model))

# By Instrument Model
error_codes %>%
  group_by(country_code, instrument_model, error_code_list) %>%
  summarize(n = n()) %>%
  group_by(country_code, instrument_model) %>%
  slice_max(order_by = n, n = 5) %>%
  write_csv(paste0(dataout, "/roche_instrument_model.csv"))

for(m in unique(error_codes$country_code)) {
  dim_plot = error_codes %>%
    group_by(country_code, instrument_model, error_code_list) %>%
    summarize(n = n()) %>%
    group_by(country_code, instrument_model) %>%
    slice_max(order_by = n, n = 5) %>%
    arrange(country_code, instrument_model, n) %>%
    filter(country_code == m) %>%
    ggplot(aes(x = reorder(error_code_list, -n), y = n, fill = error_code_list)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap( ~ instrument_model,
                scales = "free") +
    scale_fill_manual(values = si_palettes$category20) +
    si_style() +
    theme(legend.position = "none")
  
  ggsave(
    filename = paste0(dataout, "/roche_instrument_model_",  m, ".png"),
    plot = dim_plot,
    device = "png",
    width = 10,
    height = 5
  )
}

error_codes %>%
  group_by(country_code, instrument_id, error_code_list) %>%
  summarize(n = n()) %>%
  group_by(country_code, instrument_id) %>%
  slice_max(order_by = n, n = 5) %>%
  write_csv(paste0(dataout, "/roche_instrument_id.csv"))

error_codes %>%
  group_by(country_code, error_code_list) %>%
  summarize(n = n()) %>%
  group_by(country_code) %>%
  slice_max(order_by = n, n = 5) %>%
  write_csv(paste0(dataout, "/roche_country.csv"))

country_plot = error_codes %>%
  group_by(country_code, error_code_list) %>%
  summarize(n = n()) %>%
  group_by(country_code) %>%
  slice_max(order_by = n, n = 5) %>%
  ggplot(aes(x = reorder(error_code_list, -n), y = n, fill = error_code_list)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap( ~ country_code,
              scales = "free") +
  scale_fill_manual(values = si_palettes$category20) +
  si_style() +
  theme(legend.position = "none")

ggsave(
  filename = paste0(dataout, "/roche_country.png"),
  plot = country_plot,
  device = "png",
  width = 10,
  height = 5
)

error_codes %>%
  group_by(error_code_list) %>%
  summarize(n = n()) %>%
  slice_max(order_by = n, n = 5)
  
#### DATA WRANGLING - Second Pass ============================================================================

error_codes = test_activity_roche %>%
  select(country_code, instrument_id, instrument_model, test_date, error_code_list) %>%
  separate(error_code_list, c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o"), sep = ",") %>%
  pivot_longer(cols = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o"), values_to = "error_code_list") %>%
  select(-name) %>%
  mutate(instrument_model = toupper(instrument_model))

error_codes = error_codes %>%
  filter(str_detect(instrument_model, "00")) %>%
  mutate(month = round_date(test_date, unit = "month"))


error_codes_sum = error_codes %>%
  group_by(country_code, instrument_model, month, error_code_list) %>%
  summarize(n = n())

error_codes_total_test = error_codes %>%
  group_by(country_code, instrument_model, month) %>%
  summarize(tests = n())

error_codes_sum = error_codes_sum %>%
  left_join(error_codes_total_test) %>%
  filter(!is.na(error_code_list))

top_errors = error_codes_sum %>%
  filter(country_code != "ZM") %>%
  ungroup() %>%
  group_by(country_code, error_code_list) %>%
  summarize(n = max(error_percent)) %>%
  slice_max(order_by = n, n = 3) %>%
  select(-n)

for(ctr in unique(error_codes_sum$country_code)){
local_errors = top_errors %>%
  filter(country_code == ctr)

plot = error_codes_sum %>%
  filter(country_code == ctr) %>%
  filter(error_code_list %in% local_errors$error_code_list) %>%
  mutate(month = as.Date(month, "%Y-%m-%d")) %>%
  ggplot(aes(x = (month+14), y = error_percent, fill = error_code_list)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(signif(error_percent, digits = 2), "%")), nudge_y = 0.1, size = 2) +
  facet_wrap( ~ error_code_list) +
  scale_fill_manual(values = si_palettes$category20) +
  scale_x_date("Month-Year", labels = date_format("%Y-%m"), breaks = date_breaks("1 month")) +
  si_style() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90)) +
  labs(title = ctr,
       y = "Error Rate (Percent)")

ggsave(plot = plot, filename = paste0(dataout, "/", ctr, "_percent.png"), device = "png", width = 20, height = 5)
  
}


for(error in unique(top_errors$error_code_list)){

  plot = error_codes_sum %>%
    group_by(country_code, month, error_code_list) %>%
    summarize(n = sum(n, na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    mutate(error_percent = n*100/tests) %>%
    filter(country_code != "ZM") %>%
    filter(error_code_list == error) %>%
    mutate(month = as.Date(month, "%Y-%m-%d")) %>%
    ggplot(aes(x = (month+14), y = error_percent, fill = error_code_list)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(signif(error_percent, digits = 2), "%")), size = 4) +
    facet_wrap( ~ country_code) +
    scale_fill_manual(values = si_palettes$category20) +
    scale_x_date("Month-Year", labels = date_format("%Y-%m"), breaks = date_breaks("1 month")) +
    si_style() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) +
    labs(title = error,
         y = "Error Rate (Percent)")
  
  ggsave(plot = plot, filename = paste0(dataout, "/", error, "_percent.png"), device = "png", width = 20, height = 5)
  
}

for(error in unique(top_errors$error_code_list)){
  
  plot = error_codes_sum %>%
    group_by(country_code, month, error_code_list) %>%
    summarize(n = sum(n, na.rm = T),
              tests = sum(tests, na.rm = T)) %>%
    mutate(error_percent = n*100/tests) %>%
    filter(country_code != "ZM") %>%
    filter(error_code_list == error) %>%
    filter(month > as.Date("2020-11-01")) %>%
    mutate(month = as.Date(month, "%Y-%m-%d")) %>%
    ggplot(aes(x = (month+14), y = error_percent, fill = error_code_list)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(signif(error_percent, digits = 2), "%")), size = 4) +
    facet_wrap( ~ country_code) +
    scale_fill_manual(values = si_palettes$category20) +
    scale_x_date("Month-Year", labels = date_format("%Y-%m"), breaks = date_breaks("1 month")) +
    si_style() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90)) +
    labs(title = error,
         y = "Error Rate (Percent)")
  
  ggsave(plot = plot, filename = paste0(dataout, "/", error, "_percent_short.png"), device = "png", width = 20, height = 5)
  
}
