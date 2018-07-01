################################################################################
##' @title Explore data - aggregate (from the perspective of the fish and benthos)
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor
##' 
##' @date 2017-09-29
##' 
##' @log 
################################################################################

rm(list=ls(all=TRUE)) 

##### SOURCE DATA, FUNCTIONS #####

## Variance inflation functions
source("model_effort_ais/model_effort_ais_R/HighstatLibV6.R")

## For mapping
source("R/get_base_map.R")
source("R/identify_poly_pings.R")

source("R/map_adriatic_reserves.R")
pomo_list <- load_pomo()
pomo <- pomo_list[[1]]
pomo_df <- pomo_list[[2]]

library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

##### LOAD EFFORT CHANGE DATA #####
mDat <- get_model_data(df_name = "aggregate", ves_port_grid_min = 0, include_dist_port = F)
mDat %>% ggplot(aes(change_log)) + geom_density()

##### MAP VARIABLES #####
map_dat <- mDat
basemap <- get_base_map_general(map_dat)

## Map change
## Get axis limits for log change scale
change_log_abs_value <- round(max(abs(range(map_dat$change_log))), 1) + 0.1

## Change in hours
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = change_log)) + 
  scale_fill_distiller(name = "Change (log)", palette = "PiYG", direction = 1, 
                       limits = c(-change_log_abs_value, change_log_abs_value)) + 
  theme(legend.position = "bottom") + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5) 

## Distance to Pomo border
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = dist_pomo)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## Distance to shore
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = dist_shore)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5) 

## Depth
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = bathy)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = -1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## Slope
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = slope)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## persist_sum
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = persist_sum)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## S
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = S)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## Hake
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = European_hake)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

## Lobster
basemap + 
  geom_raster(data = map_dat, aes(SI_LONG, SI_LATI, fill = Norway_lobster)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5)

##### PAIRS PLOT #####
# Get correlation plot
names(map_dat)
map_dat %>% 
  select(dist_shore, dist_pomo, bathy, slope, persist_sum) %>% Mypairs()

##### VARIANCE INFLATION FACTORS #####
# With bathymetry
map_dat %>%
  select(dist_shore, dist_pomo, bathy, slope, persist_sum) %>% corvif()

# Without bathymetry
map_dat %>%
  select(dist_shore, dist_pomo, slope, persist_sum) %>% corvif()

