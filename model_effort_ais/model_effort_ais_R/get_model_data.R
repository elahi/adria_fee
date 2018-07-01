################################################################################
##' @title Get model data for change in effort
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-07-03
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE)) 

##### LOAD NECESSITIES #####

library(dplyr)
library(tidyr)

## Adriatic port data
pd_complete2 <- read.csv("workspace/pd_complete2.csv") %>% select(-X)

source("model_effort_ais/model_effort_ais_R/load_d_change.R")

##### FUNCTION #####

get_model_data <- function(effort_type = "effort_hrs", cell_total_min = 0, 
                           remove_pomo_points = FALSE, ves_port_grid_min = 3, 
                           replace_na_habitat = TRUE, 
                           include_dist_port = TRUE, ...){
  
  ## Get chosen dataset
  d_change <- load_d_change(...) %>% tbl_df()
  
  # # link nations
  if(include_dist_port == TRUE){
    d_change <- pd_complete2 %>% select(nation, port_per_trip) %>%
      left_join(d_change, ., by = "port_per_trip")
  }
  
  ## Choose effort type
  if(effort_type == "effort_hrs"){
    mDat <- d_change %>% filter(effort_type == "effort_hrs")
    print("Using effort hours")
  }
  if(effort_type == "effort_days"){
    mDat <- d_change %>% filter(effort_type == "effort_days")
    print("Using effort days")
  }
  
  ## Remove cells with less than x effort
  mDat <- mDat %>% filter(cell_total >= cell_total_min)
  print(paste("Removing cells with fewer than", cell_total_min))
  
  ## Remove points inside the pomo pit
  if(remove_pomo_points == TRUE){
    mDat <- mDat %>% filter(dist_pomo >= 0)
  }
  
  ## Remove vessel-port combinations with fewer than x grid cell observations
  if(include_dist_port == TRUE & ves_port_grid_min > 0){
    ves_port_to_keep <- mDat %>% count(VE_REF, port_per_trip) %>%
      filter(n >= ves_port_grid_min) %>% ungroup() %>% select(-n)
    mDat <- inner_join(mDat, ves_port_to_keep, by = c("VE_REF", "port_per_trip"))
    print(paste("Removing vessel-port combinations with fewer than", ves_port_grid_min, "observations"))

  }
  
  ## Replace NAs in habitat quality with zero
  if(replace_na_habitat == TRUE){
    mDat <- mDat %>% tidyr::replace_na(list(persist_sum = 0))
    print("Replaced habitat quality NAs with zero")
  }
  
  ## Get depth as negative bathymetry
  mDat <- mDat %>% mutate(depth = -bathy)
  
  # Rescale covariates
  if(include_dist_port == TRUE){
    mDat <- mDat %>%
      mutate_at(vars(dist_port, dist_shore, dist_pomo, bathy, depth, slope, persist_sum),
                funs(z = as.numeric(scale(.))))  
  }
  
  if(include_dist_port == FALSE){
    mDat <- mDat %>%
      mutate_at(vars(dist_shore, dist_pomo, bathy, depth, slope, persist_sum),
                funs(z = as.numeric(scale(.))))  
  }
  
  ## Rescale rotated x and y by max xr/yr
  mDat <- mDat %>% 
    mutate(max_xyr = max(max(xr), max(yr))) %>% 
    mutate_at(vars(xr, yr), funs(z = as.numeric(scale(., scale = F))/max_xyr))

  ## Rescale long and lat by max long/lat
  mDat <- mDat %>% 
    mutate(max_long_lat = max(max(SI_LONG), max(SI_LATI))) %>% 
    mutate_at(vars(SI_LONG, SI_LATI), 
              funs(z = as.numeric(scale(., scale = F))/max_long_lat))
  
  return(mDat)
  
}
