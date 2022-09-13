# PURPOSE: Find closest datim lab site to supplier sites.
# AUTHOR: M.kalnoky | OHA/SCH
# LICENSE: MIT
# DATE: 2022-08-04
# NOTES: 

# LOCALS & SETUP ============================================================================

    # Libraries
    library(tidyverse)
    library(geosphere)

# LOAD DATA ============================================================================  

    # load Supplier data
    df_lab  <- read_csv("Data/site_candidate_coords_selected.csv") # import results from step one
    df_lab_active  <- read_csv("Data/active_locations.csv") # import list of active supplier site id's for filtering
    
    # load Datim data import verified DATIM labs data
    df_datim_labs <- read_csv("Data/datim_lab_sites.csv") # 

# MUNGE ============================================================================
    
    #  Select only the DATIM sites in countries from where suppliers are sending data
    df_datim_labs <- df_datim_labs %>% 
          filter(countryname %in% c("Eswatini",
                       "Kenya",
                       "Nigeria",
                       "Uganda",
                       "Zambia",
                       "Zimbabwe",
                       "Mozambique"))
    
      # filter the confirmed lab data to only include the active lab sites
      df_lab <- df_lab %>% 
      filter(supplier_site_id %in% df_lab_active$supplier_site_id) 
    
      
      # create data frame with the data from the nearest datim lab facility 
      # for each supplier site
      df_closest_datim <- map_df(1:nrow(df_lab), function(.x){
      
      lat1 <- df_lab$candidate_lat[.x] 
      lon1 <- df_lab$candidate_lon[.x] 
    
      distances <- map_dbl(.x = 1:nrow(df_datim_labs), ~ {
        lat2 <-df_datim_labs$latitude[.x]
        lon2 <-df_datim_labs$longitude[.x]
        distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
      })
      
      d1 <- sort(distances)[1]
      return(data.frame(datim_name = df_datim_labs$name[which(distances == d1)[1]],
      id = df_datim_labs$id[which(distances == d1)[1]],
      distance_nearest_datim_loc_km = round(distances[which(distances == d1)[1]]/1000,2), # divide by 1000 to get km
      datim_lat = df_datim_labs$latitude[which(distances == d1)[1]],
      datim_lon = df_datim_labs$longitude[which(distances == d1)[1]]      
      ))
    })
      
      df_lab <- cbind(df_lab, df_closest_datim)

# SPIN DOWN      
      
    # This is the file that you can use to compare the site Google API returned names 
    # and the distances and to the nearest DATIM site to determine if the DATIM site and 
    # and the supplier site are referring to the same location.
      
    write_csv(df_lab, "Dataout/vldata_datim_site_matches.csv")

