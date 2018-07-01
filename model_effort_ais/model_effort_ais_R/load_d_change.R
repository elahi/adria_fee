## load effort change datasets

load_d_change <- function(df_name = "ves_port"){
  
  library(dplyr)
  
  ## source rotation functions
  source("model_effort_ais/model_effort_ais_R/rotate_functions.R")
  
  ## Load data
  if(df_name == "ves_port"){
    load("workspace/model_data_revised/d_change_ves_port.RData")
  }
  
  if(df_name == "aggregate"){
    load("workspace/model_data_revised/d_change_aggregate.RData")
  }
  
  if(df_name == "aggregate_vesci"){
    load("workspace/model_data_revised/d_change_aggregate_vesci.RData")
  }
  
  ## Calculate total hours per cell
  d_change <- d_change %>% mutate(cell_total = before + after)
  
  ## Get rotated xy
  xy <- d_change %>% get_utm_xy()
  xyr <- rotate_xy(xy)
  
  d_change <- cbind(d_change, xyr) 
  
  return(d_change)
}
