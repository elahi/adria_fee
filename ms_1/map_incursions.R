################################################################################
##' @title Map fishing incursions
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

## Impacted vessels
source("ms_1/plot_time_series_ais_incursions.R")
ves_ci %>% count(ves_ci)
ves_port_suspects_plot

## Effort data
load("workspace/ais_summary_data_revised/ais_hrs_grid_ves_incursions.RData")
mDat <- d_change_incursions 
mDat

## Join with ves_ci
mDat <- ves_ci %>% 
  left_join(mDat, ., by = "VE_REF")
mDat %>% count(ves_ci)
mDat <- mDat %>% 
  mutate(ves_ci = ifelse(is.na(ves_ci), "Other", ves_ci))

## Suspect vessels
ves_suspects <- read.csv("output/ves_suspects_df.csv")
ves_suspects <- ves_suspects %>% slice(1:9) %>% 
  mutate(VE_REF = as.character(VE_REF)) %>% 
  select(-X)

## Join
mDat <- left_join(mDat, ves_suspects, by = "VE_REF") %>% 
  mutate(VE_REF = factor(VE_REF)) %>% 
  mutate(VE_REF = reorder(VE_REF, sum_after_ves, min))
levels(mDat$VE_REF)

##### GET CONVEX HULL FOR STUDY AREA #####
## Coords of study area
coords <- read.csv("workspace/process_effort_output/study_area_coords.csv") %>% select(-X)
source("R/fortify_shapefile.R")

## Get convex hull polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
# Match the projection to the map of the adriatic sea [wgs84]
proj4string(sp_poly) <- CRS("+proj=longlat +datum=WGS84")
hull_coords <- fortify(sp_poly)

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

ves_suspects_plot <- ves_suspects %>% slice(1:4) 
ves_suspects_plot

test_dat <- mDat[mDat$VE_REF %in% ves_suspects_plot$VE_REF, ]

vpsp <- ves_port_suspects_plot[ves_port_suspects_plot$VE_REF %in% ves_suspects_plot$VE_REF, ]

vpsp <- vpsp %>% 
  mutate(port_upper = toupper(port_per_trip))

vpsp <- pd_complete2 %>% rename(port_upper = port_per_trip) %>% 
  left_join(vpsp, ., by = "port_upper")

test_dat <- test_dat %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after"), 
                                 labels = c("Before closure", "After closure")), 
         ves_ci = factor(ves_ci, labels = c("Impacted vessels",
                                            "Other vessels")))
names(test_dat)
test_dat <- droplevels(test_dat)
levels(test_dat$VE_REF)

test_dat <- test_dat %>% 
  mutate(VE_REF = factor(VE_REF, levels = rev(levels(VE_REF))))

## Filter to After closure
test_dat <- test_dat %>% filter(reserve_timing == "After closure")

basemap <- get_base_map_general(test_dat, range_extension = 0.01, my_color = "black")

## Facet labels to match maps
facet_labels_map <- facet_labels[facet_labels$VE_REF %in% ves_suspects_plot$VE_REF, ]

## Time-series theme
theme_set(theme_bw() + 
            theme(panel.grid = element_blank(), 
                  strip.background = element_blank(),
                  strip.text.x = element_blank(), 
                  #axis.text.y = element_text(angle = 90, hjust = 0.5),
                  legend.position = "top", 
                  legend.background = element_blank(), 
                  #legend.title = element_blank(),
                  legend.key = element_blank()))

basemap + 
  geom_raster(data = test_dat, aes(SI_LONG, SI_LATI, fill = effort_hrs)) + 
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       trans = "log10", 
                       #breaks = c(1, 5, 10, 15), 
                       na.value = "white", 
                       name = "Effort\n(hours)") + 
  theme(legend.position = "top") + 
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = seq(-25, -275, by = -50), size = 0.05, alpha = 0.5) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.25, alpha = 0.75) + 
  facet_wrap(~ VE_REF) +
  geom_point(aes(SI_LONG, SI_LATI, size = sum, fill = NULL), 
             data = vpsp, alpha = 1, fill = "black", pch = 21) +
  scale_size(range = c(0.5,2), breaks = c(50, 100, 150), name = "Hours") + 
  geom_text(aes(SI_LONG, SI_LATI, label = port_per_trip), 
             data = vpsp, nudge_y = -0.2, nudge_x = 0.05, size = 2) + 
  geom_text(aes(18.75, 43.5, label = VE_REF3, color = NULL, shape = NULL), 
            data = facet_labels_map, hjust = 0, size = 2.5, fontface = "bold") 

ggsave("ms_1/ms_1_figs/map_incursions.png", height = 5, width = 5, dpi = 300)

