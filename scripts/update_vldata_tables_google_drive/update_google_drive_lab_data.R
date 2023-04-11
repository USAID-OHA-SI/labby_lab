# PURPOSE: This script is for downloading the tables from the vldata database and
# uploading the files as tables to Google Drive.
# AUTHOR: M.Kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES:  

# LOCALS & SETUP ============================================================================

      # Libraries
      library(tidyverse)
      library(googledrive)
      library(purrr)
      library(DBI)
      library(RPostgres)

# LOAD DATA ============================================================================  
 
      # Authorization to download data from Google Drive.  
      # request authorization from Google. If you are connecting to Google drive form R using the googledrive package for the first time
      # you can find instruction on how to gain Authorization here https://googledrive.tidyverse.org/.  
      
      drive_auth()
      
      dsn_database = 'vlprddb'   # Specify the name of your database.
      dsn_hostname = 'vl-prd-db.postgres.database.azure.com'  
      dsn_port = ''                # Specify your port number as a character. e.g. 5432
      dsn_uid = ''   # Specify your username. e.g. "admin"
      dsn_pwd = ''        # Specify your password. e.g. "xxx"
      
      # establish a connection to the database.
      connec <- DBI::dbConnect(RPostgres::Postgres(), 
                               dbname = dsn_database,
                               host = dsn_hostname, 
                               port = dsn_port,
                               user = dsn_uid, 
                               password = dsn_pwd)
      
      # load tables from vldata and stgdata schema of vlprddb database as a list of dataframes.
      vldata_tables_to_download <- dbGetQuery(connec, "SELECT table_name FROM information_schema.tables
                         WHERE table_schema='vldata'")
      
      stgdata_tables_to_download <- dbGetQuery(connec, "SELECT table_name FROM information_schema.tables
                         WHERE table_schema='stgdata'")
      
      vldata_tables <- purrr::map(.x = vldata_tables_to_download$table_name, ~ dbGetQuery(connec, paste0("SELECT * FROM vlprddb.vldata.",.)))
      stgdata_tables <- purrr::map(.x = stgdata_tables_to_download$table_name, ~ dbGetQuery(connec, paste0("SELECT * FROM vlprddb.stgdata.",.)))
      
      # disconnect from vlprddb
      dbDisconnect(connec)

# MUNGE ============================================================================

    # combined lists containing all our data from vldata and staging schemas
    all_tables <- c(vldata_tables, stgdata_tables)
      
    # combine all table names
    all_table_names <- c(vldata_tables_to_download$table_name , stgdata_tables_to_download$table_name)
    
    # save files to csv's (we can't directly save a csv in Google Drive from a data frame)
    walk(.x = 1:length(all_tables), ~ write_csv(all_tables[[.]], file = paste0(all_table_names[.],".csv")))
    
    # Upload all our csv's to the vldata_tables_download folder in Google Drive)
    walk(1:length(all_tables), ~ drive_upload(paste0(all_table_names[.],".csv"), 
                 path = as_id("198YgVgnr9_z6YhqbQWbDlo0GJK70u_cV"),
                 overwrite = TRUE))


   # delete the csv files created
    walk(list.files(pattern = ".csv"), ~ unlink(.))
    
   # All done! Check if they've loaded in Google drive. 
