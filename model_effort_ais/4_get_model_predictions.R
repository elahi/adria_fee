################################################################################
##' @title Get partial effects from bam - aggregate effort hours
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-09-27
##' 
##' @log Add a log here
################################################################################

# rm(list=ls(all=TRUE)) 

##### LOAD NECESSITIES #####

library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

# Source gam prediction functions
source("model_effort_ais/model_effort_ais_R/predict_gam_tidy.R")

## Load bam models
load(file = "workspace/model_data_revised/bam_1.RData")
load(file = "workspace/model_data_revised/bam_2.RData")
load(file = "workspace/model_data_revised/bam_3.RData")

summary(bam_1) # timing*dist_pomo + timing*dist_shore + timing*depth + timing*persist + timing*slope
summary(bam_2) # timing*dist_pomo + timing*dist_shore + timing*persist + timing*slope
summary(bam_3) # timing*dist_shore + timing*depth + timing*persist + timing*slope

AIC(bam_1, bam_2, bam_3)

m_check <- bam_2

##### LOAD EFFORT CHANGE DATA #####
mDat <- get_model_data(df_name = "aggregate", ves_port_grid_min = 0, 
                       include_dist_port = F)

## Get in long format again - by reserve_timing
mDat2 <- mDat %>% gather(key = reserve_timing, value = effort, before:after)

## Minimum hours to add for gamma distribution
min_hrs <- mDat2 %>% filter(effort > 0) %>% .$effort %>% quantile(., 0.01)

mDat2 <- mDat2 %>% 
  mutate(effort_plus = effort + min_hrs, 
         effort_log = log10(effort_plus)) %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after")))

test_dat <- mDat2

## To back-calculate z_scores
## Get mean and sd values
mean_df <- test_dat %>% 
  summarise_at(vars(dist_shore, dist_pomo,
                    depth, 
                    slope, persist_sum), 
               funs(metric = as.numeric(mean(.)))) %>% 
  gather(key = label, value = mean) 

sd_df <- test_dat %>% 
  summarise_at(vars(dist_shore, dist_pomo,
                    depth, 
                    slope, persist_sum), 
               funs(metric = as.numeric(sd(.)))) %>% 
  gather(key = label, value = sd)

mean_sd_df <- inner_join(mean_df, sd_df) %>% 
  mutate(xlab = sub(pattern = "_metric", x = label, replacement = "_z"))

##### PARTIALS BY HAND #####
test_dat

### Prepare data
new_dat_cols <- test_dat %>% 
  select(dist_shore_z, dist_pomo_z, depth_z, slope_z, SI_LONG_z, SI_LATI_z, persist_sum_z)
new_data_rows = 100

## Make new data for these columns
new_data <- make_new_data(new_dat_cols)

## Get median values
med_z <- new_dat_cols %>% summarise_all(funs(med = as.numeric(median(.))))
median_data <- make_median_data(new_dat_cols, new_data_rows = 100)

## Make predictions
all_predictors <- unique(new_data$xlab)

# Select one predictor
i = 1
new_predictor <- all_predictors[i]

new_df <- make_pred_df(new_data = new_data, 
                       median_data = median_data, new_predictor = new_predictor)

## Make predictions
new_df2 <- predict_gam_tidy(new_df = new_df, new_predictor = new_predictor)

# Complete for others using loop
for(i in 2:length(all_predictors)){
  new_predictor <- all_predictors[i]
  
  new_df_i <- make_pred_df(new_data = new_data, 
                           median_data = median_data, new_predictor = new_predictor)
  
  ## Make predictions
  new_df_i <- predict_gam_tidy(new_df = new_df_i, new_predictor = new_predictor)
  
  ## Update dataframe
  new_df2 <- rbind(new_df2, new_df_i)
}

names(new_df2)

## Join dfs
new_df2 <- mean_sd_df %>% select(-label) %>%
  left_join(new_df2, ., by = "xlab") 

## Back-calculate
new_df2 <- new_df2 %>% mutate(x = (x_z * sd) + mean)

