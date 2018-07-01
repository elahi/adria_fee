################################################################################
##' @title Map predictors
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####
## Load packages
library(dplyr)
library(tidyr)
library(cowplot)
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

mDat <- get_model_data(df_name = "aggregate", ves_port_grid_min = 0, 
                       include_dist_port = F)

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

##### MAP PREDICTORS #####
test_dat <- mDat %>% 
  mutate(Depth = -bathy) %>%
  rename(`Distance to Pomo` = dist_pomo, 
         `Distance to shore` = dist_shore, 
         `Bottom slope` = slope)

basemap <- get_base_map_general(test_dat, range_extension = 0.1, my_color = "black", my_fill = "gray")

map_a_predictor <- function(x, fill_string = "none"){
  
  basemap + 
    geom_raster(data = x, aes_string(x = "SI_LONG", y = "SI_LATI", fill = fill_string)) + 
    scale_fill_distiller(palette = "YlOrRd", direction = 1) + 
    theme(legend.position = "top") + 
    geom_path(data = hull_coords, aes(long, lat), alpha = 0.75) + 
    geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
                 breaks = seq(-25, -275, by = -50), size = 0.1, alpha = 0.5) + 
    geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5) + 
    theme(legend.box.margin = margin(t=0, r=0, b=0, l=0, unit="cm"), 
          legend.spacing = unit(0, "cm"))
  
}

plot_dist_pomo <- map_a_predictor(x = test_dat, fill = "`Distance to Pomo`")
plot_dist_shore <- map_a_predictor(x = test_dat, fill = "`Distance to shore`")
plot_bathy <- map_a_predictor(x = test_dat, fill = "Depth")
plot_slope <- map_a_predictor(x = test_dat, fill = "`Bottom slope`")

map_predictors <- plot_grid(plot_dist_pomo, plot_dist_shore, 
                            plot_bathy, plot_slope, ncol = 2)

save_plot("ms_1/ms_1_figs/map_predictors.png", map_predictors, 
          ncol = 2, nrow = 2, 
          base_height = 3.5, base_width = 3.5)

##### MAP NURSERIES #####
names(test_dat)

tdl <- test_dat %>% 
  select(SI_LONG, SI_LATI, persist_sum, 
         Broadtail_shortfin_squid, Deepwater_rose_shrimp, 
         European_hake, Horned_octopus, Norway_lobster) %>% 
  rename(Habitat_quality = persist_sum) %>% 
  gather(key = spp, value = value, Habitat_quality:Norway_lobster) %>% 
  mutate(spp = relevel(factor(spp), ref = "Habitat_quality")) 
summary(tdl)

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))
basemap + 
  geom_raster(data = tdl, aes(SI_LONG, SI_LATI, fill = value)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       trans = "log10", name = "Persistence", 
                       breaks = c(1, 2, 3, 4, 5, 10, 19), na.value = "white") + 
  theme(legend.position = "top") + 
  geom_path(data = hull_coords, aes(long, lat), alpha = 0.75) + 
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = seq(-25, -275, by = -50), size = 0.1, alpha = 0.5) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.5) + 
  facet_wrap(~ spp, ncol = 2) 

ggsave("ms_1/ms_1_figs/map_nurseries.png", width = 6, height = 8)