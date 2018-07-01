################################################################################
##' @title Model aggregate data - effort hours
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-09-29
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE)) 

##### LOAD NECESSITIES #####

library(dplyr)
library(tidyr)
library(mgcv)

## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

##### LOAD EFFORT CHANGE DATA #####
mDat <- get_model_data(df_name = "aggregate", ves_port_grid_min = 0, 
                       include_dist_port = F)

## Get in long format again - by reserve_timing
mDat2 <- mDat %>% gather(key = reserve_timing, value = effort, before:after)
summary(mDat2)
mDat2 %>% filter(effort == 0) %>% count(reserve_timing)
mDat2 %>% filter(effort > 0) %>% .$effort %>% quantile(., seq(0, 0.1, by = 0.01))

min_hrs <- mDat2 %>% filter(effort > 0) %>% .$effort %>% quantile(., 0.01)

mDat2 <- mDat2 %>% 
  mutate(effort_plus = effort + min_hrs, 
         effort_log = log10(effort_plus)) %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after")))

##### BAM - HOURS BY RESERVE TIMING - GAMMA DISTRIBUTION #####
test_dat <- mDat2
names(test_dat)
summary(test_dat)

bam_1 <- bam(effort_plus ~ s(SI_LONG_z, SI_LATI_z) + 
                   s(dist_pomo_z, by = reserve_timing) + 
                   s(dist_shore_z, by = reserve_timing) +
                   s(depth_z, by = reserve_timing) +
                   reserve_timing * persist_sum_z +
                   reserve_timing * slope_z, 
                 family = Gamma(link = "log"), data = test_dat)

bam_2 <- bam(effort_plus ~ s(SI_LONG_z, SI_LATI_z) + 
               s(dist_pomo_z, by = reserve_timing) + 
               s(dist_shore_z, by = reserve_timing) +
               #s(depth_z, by = reserve_timing) +
               reserve_timing * persist_sum_z +
               reserve_timing * slope_z, 
             family = Gamma(link = "log"), data = test_dat)

bam_3 <- bam(effort_plus ~ s(SI_LONG_z, SI_LATI_z) +
               #s(dist_pomo_z, by = reserve_timing) +
               s(dist_shore_z, by = reserve_timing) +
               s(depth_z, by = reserve_timing) +
               reserve_timing * persist_sum_z +
               reserve_timing * slope_z,
             family = Gamma(link = "log"), data = test_dat)

AIC(bam_1, bam_2, bam_3)

save(bam_1, file = "workspace/model_data_revised/bam_1.RData", compress = "xz")
save(bam_2, file = "workspace/model_data_revised/bam_2.RData", compress = "xz")
save(bam_3, file = "workspace/model_data_revised/bam_3.RData", compress = "xz")

