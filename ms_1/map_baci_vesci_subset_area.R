################################################################################
##' @title Map hours by reserve timing and vessel status
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####
## Load packages
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
library(marmap)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## For mapping
source("R/get_base_map.R")
source("R/map_adriatic_reserves.R")
source("R/identify_adriatic_points.R")
pomo_list <- load_pomo()
pomo <- pomo_list[[1]]
pomo_df <- pomo_list[[2]]

## Adriatic port data
pd_complete2 <- read.csv("workspace/pd_complete2.csv") %>% select(-X)

##### GET CONVEX HULL FOR STUDY AREA #####
## Coords of study area
coords <- read.csv("workspace/process_effort_output/study_area_coords.csv") %>% select(-X)
source("R/fortify_shapefile.R")

## Get convex hull polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
# Match the projection to the map of the adriatic sea [wgs84]
proj4string(sp_poly) <- CRS("+proj=longlat +datum=WGS84")
hull_coords <- fortify(sp_poly)

##### LOAD EFFORT CHANGE DATA #####
## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

mDat <- get_model_data(df_name = "aggregate_vesci", ves_port_grid_min = 0, 
                       include_dist_port = F)
unique(mDat$ves_ci)
mDat %>% count(ves_ci)

## Get in long format again - by reserve_timing
mDat2 <- mDat %>% gather(key = reserve_timing, value = effort, before:after)
names(mDat2)

##### GET BATHYMETRY #####
## Get bathymetric data for the same region
## Resolution is in minutes
adria_bath <- getNOAA.bathy (lon1 = 12, lon2 = 20, 
                             lat1 = 39, lat2 = 46, resolution = 1) 

### Fortify bathy object and use standard ggplot2
adria_df <- fortify.bathy(adria_bath) %>% 
  rename(SI_LONG = x, SI_LATI = y)

### Filter to adriatic points only
adria_df <- identify_adriatic_pings(adria_df)

##### MAP IT #####
test_dat <- mDat2 %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after"), 
                                 labels = c("Before closure", "After closure")), 
         ves_ci = factor(ves_ci, labels = c("Control vessels", 
                                            "Impacted vessels",
                                            "Other vessels")))
names(test_dat)

basemap <- get_base_map_general(test_dat, range_extension = 0.1, my_color = "black")

basemap + 
  geom_raster(data = test_dat, aes(SI_LONG, SI_LATI, fill = effort)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       trans = "log10", na.value = "white", 
                       name = "Effort\n(hours)") + 
  theme(legend.position = "top") + 
  geom_path(data = hull_coords, aes(long, lat), alpha = 0.75) + 
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = seq(-25, -275, by = -50), size = 0.1, alpha = 0.5) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5) + 
  facet_grid(ves_ci ~ reserve_timing) 

ggsave("ms_1/ms_1_figs/map_baci_hrs_vesci.png", height = 7, width = 5)

