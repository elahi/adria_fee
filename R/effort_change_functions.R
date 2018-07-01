################################################################################
##' @title Functions to estimate change in effort
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-01-10
##' 
##' @log Add a log here
################################################################################

library(dplyr)
library(tidyr)

##' These functions are used to summarise effort in hours

## Calculate total effort in hours per group
sum_hours_per_group <- function(x, ...){
  x %>%
    group_by(...) %>% 
    summarise(effort_hrs = sum(sum, na.rm = TRUE)) %>%
    ungroup()
}  

## Function to replace NAs with zero for a column
replace_na_re <- function(column_x, replacement_value = 0){
  column_x[is.na(column_x)] <- replacement_value
  return(column_x)
}

##' Function to calculate absolute change and log change
##' Requires before and after columns
##' add_value = is the amount to add to zero effort for LRR
##' (default = 1 minute, 1/60)
get_effort_change <- function(x, add_value = 1/60){
  x <- x %>% 
    mutate(before2 = before + add_value, 
           after2 = after + add_value, 
           change_abs = after - before, 
           change_log = log10(after2/before2), 
           bef_log = log10(before2), 
           aft_log = log10(after2))%>% 
    select(-c(before2, after2))
  return(x)
}

# ##' Function to get effort change dataframe, with option to subset vessels
# get_effort_change_df <- function(x, min_percentile = 0.01, subset_vessels = FALSE, 
#                                  subset_vessel_df = ves_ci_df, ...){
#   
#   ## Get minimum effort (to add to zeros for lrr)
#   min_effort <- x %>% 
#     summarise(min_effort = quantile(effort_hrs, min_percentile, na.rm = TRUE)) %>% 
#     unlist(use.names = F)
#   
#   ## Subset vessels according to another dataframe that has VE_REF in it
#   if(subset_vessels == TRUE){
#     x2 <- x[x$VE_REF %in% subset_vessel_df$VE_REF, ]
#     ## Get in wide format
#     dat <- x2 %>% spread(key = reserve_timing, value = effort_hrs)
#   }
#   
#   if(subset_vessels == FALSE){
#     dat <- x %>% spread(key = reserve_timing, value = effort_hrs)
#   }
#   
#   ## Replace NAs in before-after columns
#   dat <- dat %>% 
#     mutate(before = replace_na(before), after = replace_na(after))
#   
#   ## Calculate change in effort
#   dat <- get_effort_change(dat, add_value = min_effort)
#   
#   return(dat)
#   
# }

##' Function to get effort change dataframe
##' x = dataset with effort
get_effort_change_df <- function(x, min_percentile = 0.01){

  ## Get minimum effort (to add to zeros for lrr)
  min_effort <- x %>% 
    summarise(quantile(effort, min_percentile, na.rm = TRUE)) %>% 
    unlist(use.names = F)
  
  ## Get effort change df
  x <- x %>% spread(key = reserve_timing, value = effort)

  ## Replace NAs in before-after columns
  x <- x %>% 
    mutate(before = replace_na_re(before), after = replace_na_re(after))
  
  ## Calculate change in effort
  x <- get_effort_change(x, add_value = min_effort)
  
  return(x)
  
}
