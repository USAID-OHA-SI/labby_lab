# PURPOSE: This code is to create summary graphs and tables of the total number
# of failed patient samples due to reagent / kit related errors on the Abbott
# m2000 platform
# in the last year by test_type and country.
# AUTHOR: M.kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES: This version of the code downloads error code data from Google Drive.
#        as well as error data from the vldata.error table in the lab database

# LOCALS & SETUP ===============================================================

    # Libraries
    library(tidyverse)
    library(purrr)
    library(googledrive)
    library(gt)
    library(glue)
    library(DBI)
    library(RPostgres)
    library(ggplot2)
    library(glitr)
    library(extrafont)



# LOAD DATA ============================================================================  

    # Authorization to download data from Google Drive.  
    # request authorization from Google. If you are connecting to Google drive form R using the googledrive package for the first time
    # you can find instruction on how to gain Authorization here https://googledrive.tidyverse.org/.  
    
    drive_auth()
    
    # download the instrument error codes csv file from the Google Drive
    
    folder <- drive_get(as_id("1oC3EL_XQHebN7-6CXM2f8pNGkiv3O-5i"))
    csv_files <- drive_ls(folder, type = "csv")
    
    # download them
    
    path <- paste0(getwd(),"/Data")
    csv_files %>% 
      split(csv_files$id) %>% 
      walk(~ drive_download(.$id, path = file.path(path, .$name), overwrite = TRUE))
    
    
    # load all our files
    
    csv_files <- fs::dir_ls("Data/", regexp = "\\.csv$")
    error_code_desc <- purrr::map(csv_files, read_csv)
    

    # credentials to log into the vl database.  
    dsn_database = 'database name'   # Specify the name of your database.
    dsn_hostname = 'host name'  
    dsn_port = 'port number'                # Specify your port number as a character. e.g. 5432
    dsn_uid = 'username'   # Specify your username. e.g. "admin"
    dsn_pwd = 'password'        # Specify your password. e.g. "xxx"
    
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
    test <- dbGetQuery(connec, "SELECT * FROM vlprddb.stgdata.test_activity_abbott")
    country <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.country")
    
    
# MUNGE ============================================================================   

    # get the error description for reagent / kit type errors 
    df_error_desc <- error_code_desc$`Data/reagent_kit_error_codes_m2000sp_m2000rt.csv`
    
    # create data frame that combines the error log data to supplier information and 
    # instrument information
    
    df_error_log <- instrument_master %>% 
      left_join(site, by = "supplier_site_id") %>%
      left_join(country, by = "country_code") %>%
      mutate(instrument_model = iconv(instrument_model, "latin1", "ASCII", sub="")) %>%  # transformations to help standardize instrument names 
      mutate(instrument_model = gsub(","," ", instrument_model)) %>% # transformations to help standardize instrument names 
      mutate(instrument_model = str_squish(instrument_model)) %>% # transformations to help standardize instrument names 
      mutate(instrument_model = str_to_lower(instrument_model)) %>% # transformations to help standardize instrument names 
      right_join(test, by = "instrument_id") %>%
      filter(instrument_model == "m2000rt", # just keep m2000 data because there may be same error codes across different instruments. 
             error_type_id %in% c(df_error_desc$error_code)) %>% 
      mutate(error_type_id = as.numeric(error_type_id)) %>% 
      left_join(df_error_desc, by = c("error_type_id" = "error_code")) %>% 
      select(instrument_id,
             instrument_model,
             supplier_site_id,
             country_name, 
             test_record_id,
             test_type_id,
             error_type_id,
             error_count,
             test_date,
             is_patient_test,
             is_control_test,
             patient_test_error_flag,
             control_test_error_flag,
             excluded_tests, 
             category_message,
             possible_issue,
             protocol)


# VIZ ============================================================================        

    # Make a quick plot to see proportions of error modes
    df_error_log %>% 
      group_by(error_type_id) %>% 
      mutate(tot_err = sum(error_count)) %>% 
      distinct(error_type_id, .keep_all = TRUE) %>% 
      ggplot(aes(x = reorder(error_type_id, tot_err), y=tot_err)) +
      geom_bar(stat="identity", width=0.5) + 
      coord_flip() +
      xlab("m2000rt total error codes since Dec 2018.") +
      ylab("Count") +
      si_style() +
      ggsave("Graphics/error_barplot.png")


# make table to see error code descriptions

    df_error_log %>% 
      group_by(error_type_id) %>% 
      mutate(tot_err = sum(error_count)) %>% 
      distinct(error_type_id, .keep_all = TRUE) %>%   
      arrange(-tot_err) %>% 
      rename(`Total Errors` = tot_err) %>% 
      rename(`Error Code` = error_type_id) %>% 
      rename(`Error Category` = category_message) %>% 
      select(`Error Category`,
             `Error Code`,
             `Total Errors`) %>% 
      gt() %>%
      tab_header(
        title = "Total number of m2000rt error codes since 2018.",
      ) %>% 
      gtsave("Images/error_descriptions_table.pdf", expand = 15)

    # How many samples are lost because of 4 most frequent control errors?
    # when a control fails the whole plate fails.
    # These 4 errors account for 90% of all the errors related to kit / reagents 4442, 4457, 4458, 4441
    # We want to figure out how many plates were affected by at least one of these errors.
    # The first four elements of test_record_id that are separated by a pipe make up the plate barcode.
    # The fifth part of test_record_id is the well number.
    
    
    df_affected_plates_country <- instrument_master %>% 
      left_join(site, by = "supplier_site_id") %>%
      left_join(country, by = "country_code") %>%
      mutate(instrument_model = iconv(instrument_model, "latin1", "ASCII", sub="")) %>%  # transformations to help standardize instrument names 
      mutate(instrument_model = gsub(","," ", instrument_model)) %>% # transformations to help standardize instrument names 
      mutate(instrument_model = str_squish(instrument_model)) %>% # transformations to help standardize instrument names 
      mutate(instrument_model = str_to_lower(instrument_model)) %>% # transformations to help standardize instrument names 
      right_join(test, by = "instrument_id") %>%
      filter(instrument_model == "m2000rt") %>%  # just keep m2000 data because there may be same error codes across different instruments. 
      separate(col = test_record_id, sep = "\\|", into = c("date","test_type","barcode","log","sample_number"), remove=FALSE) %>% 
      mutate(plate = paste(date,test_type,barcode,log)) %>%  # create a unique plate barcode
      group_by(country_name, plate) %>% 
      summarise(control_errors = ifelse(length(which(error_type_id %in% c("4442", "4457", "4458", "4441"))) > 0, "affected", "non-affected"))
        
    
      table(df_affected_plates_country$country_name, df_affected_plates_country$control_errors)      
            
            
            










