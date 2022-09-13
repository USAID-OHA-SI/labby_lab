# PURPOSE: Some code to help find the geo coordinates each site by entering 
# the site, city and country names into google ways as an query.
# Googleways is an R package that queries the Google Maps API.
# AUTHOR: M.Kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-07-24
# NOTES: This version of the code connects directly to vlprddb. 

# LOCALS & SETUP ============================================================================

  # Libraries
  library(tidyverse)
  library(DBI)
  library(RPostgres)
  library(googleway)
  library(ggmap)

# LOAD DATA ============================================================================  

  # Register Google Maps API key
  register_google(key = "google_key", write = TRUE)
  key <- "google_key"

  # credentials to log into the database.  
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
  
  # download site table from vldata schema of vlprddb database.
  site <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.site")
  country <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.country")

# MUNGE ============================================================================

  # The strategy is to create a query linked to each site_id, then to search
  # for the geographic coordinates of each unique site.The code will return a table 
  # of candidate locations the Google search returns. A person can then go through the
  # the table and evaluate which if any of the candidates is a match for the true site location.
  
  df_site_w_address <- site %>% 
              left_join(country, by = "country_code") %>%  # better to use the country_name in address than country_id.
              mutate(site_name_city_country = paste(supplier_site_name, city, country_name)) %>% 
              mutate(site_name_city_country = iconv(site_name_city_country, "latin1", "ASCII", sub="")) %>% 
              mutate(site_name_city_country = str_squish(site_name_city_country)) %>% 
              mutate(query = paste(site_name_city_country,"laboratory"))

  
  site_list <- map(.x = 1:nrow(df_site_w_address), ~ {
    
    res <- google_find_place(
      input = df_site_w_address$query[.],
      inputtype = "textquery",
      key = key
    )
    
    df_site_candidates <- bind_cols(df_site_w_address$supplier_site_id[.], 
                                    df_site_w_address$site_name_city_country[.],
                                    res$candidates$name,
                                    res$candidates$formatted_address,
                                    res$candidates$geometry$location) 
    
    colnames(df_site_candidates) <- c("supplier_site_id", "supplier_site_name_city_country",
                                      "candidate_name", "candidate_address", "candidate_lat","candidate_lon")
    df_site_candidates
  })


  
  # unlist all the data frames and bind by rows
    site_candidate_coords <- do.call(bind_rows, site_list)

# SPIN DOWN  ============================================================================
  
    # This file will contain all the potential matches returned by the Google Maps API 
    # for each of the sites. The analyst will need to go in and select the best choice for
    # site and create a new document with the single best selection for each site as well as
    # add a new column called api_match with values (1- a match has been found) and 
    # (0 - no match has been found). Save this file in the data folder as 
    # site_candidate_coords_selected.csv then proceed to step 2.
    
    write_csv(site_candidate_coords, "Dataout/site_candidate_coords.csv")


    






