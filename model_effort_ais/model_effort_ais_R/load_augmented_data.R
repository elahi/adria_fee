################################################################################
##' @title Load data augmented with model residuals and fitted values
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-07-18
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

library(broom)
library(dplyr)
library(mgcv)

## Load final bam model
load(file = "workspace/model_data_revised/bam_2.RData")

# model to check
m_check <- bam_2
m_check

## Adriatic port data
pd_complete2 <- read.csv("workspace/pd_complete2.csv") %>% select(-X)

##### LOAD EFFORT CHANGE DATA #####
## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

mDat <- get_model_data(df_name = "aggregate", ves_port_grid_min = 0, 
                       include_dist_port = F)

## Get in long format again - by reserve_timing
mDat2 <- mDat %>% gather(key = reserve_timing, value = effort, before:after)
min_hrs <- mDat2 %>% filter(effort > 0) %>% .$effort %>% quantile(., 0.01)

mDat2 <- mDat2 %>% 
  mutate(effort_plus = effort + min_hrs, 
         effort_log = log10(effort_plus)) %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after")))

## Augment the dataset with fitted and residual values
test_dat <- mDat2
m_aug <- test_dat %>% 
  mutate(.fitted = fitted(m_check), .resid = resid(m_check), 
         pomo = ifelse(dist_pomo >= 0, "outside_closure", "inside_closure"))

## Get in long format
names(m_aug)
m_augL <- m_aug %>% 
  rename(Distance_to_shore = dist_shore, 
         Distance_to_Pomo = dist_pomo, 
         Latitude = SI_LATI, 
         Longitude = SI_LONG, 
         Slope = slope, Depth = depth, 
         Fitted = .fitted, Residuals = .resid) %>% 
  mutate(Habitat_quality = persist_sum/25 * 100) %>% 
  gather(key = variable_p, value = value_p, 
         c(Distance_to_shore, Distance_to_Pomo, 
           Latitude, Longitude, Fitted, 
           Slope, Depth, Habitat_quality)) %>% 
  mutate(variable_p = factor(variable_p, 
                             levels = c("Longitude", "Latitude", 
                                        "Distance_to_shore", "Distance_to_Pomo", 
                                        "Depth", "Slope", 
                                        "Habitat_quality", "Fitted")))

