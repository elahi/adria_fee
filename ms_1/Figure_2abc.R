################################################################################
##' @title Figure 2abc
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
library(ggplot2)
library(ggrepel)
library(marmap)
library(legendMap)
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
coords <- read.csv("workspace/process_effort_output/study_area_coords.csv") %>% 
  select(-X)
source("R/fortify_shapefile.R")

## Get convex hull polygon
sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
# Match the projection to the map of the adriatic sea [wgs84]
proj4string(sp_poly) <- CRS("+proj=longlat +datum=WGS84")
hull_coords <- fortify(sp_poly)

##### LOAD PORT EFFORT DATA #####
## Port effort change data
load("workspace/model_data_revised/d_change_port.RData")
d_change_port <- d_change_port %>% filter(effort_type == "effort_hrs") %>% 
  mutate(my_scale = "Adriatic")

load("workspace/model_data_revised/d_change_port_subset_area.RData")
d_change_port_subset_area <- d_change_port_subset_area %>% filter(effort_type == "effort_hrs") %>% 
  mutate(my_scale = "Study_area")

min_percentile = 0.01
d_change_port_subset_area %>% filter(before > 0) %>% summarise(quantile(before, min_percentile, na.rm = TRUE))
d_change_port_subset_area %>% filter(after > 0) %>% summarise(quantile(after, min_percentile, na.rm = TRUE)) 

## Combine before data for entire adriatic and subset area for each port
dcp <- rbind(d_change_port, d_change_port_subset_area)
dcp <- dcp %>% select(port_per_trip, before, after, change_log, my_scale) %>% 
  mutate(total_hrs = before + after)
dcp_plot <- dcp %>% select(port_per_trip, total_hrs, my_scale) %>% 
  spread(key = my_scale, value = total_hrs) %>% filter(!is.na(Study_area))

## Link to change_log for the study area
dcp_plot <- d_change_port_subset_area %>% select(port_per_trip, change_log) %>% 
  left_join(dcp_plot, by = "port_per_trip")

## Link to change_log for the adriatic
dcp_plot_adria <- d_change_port %>% select(port_per_trip) %>% 
  left_join(dcp_plot, by = "port_per_trip")
dcp_plot_adria <- dcp_plot_adria[dcp_plot_adria$port_per_trip %in% dcp_plot$port_per_trip, ]

##### LOAD EFFORT CHANGE DATA #####
## Source get_model_data
source("model_effort_ais/model_effort_ais_R/get_model_data.R")

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

## What was the average change inside vs outside the closure?
mDat <- mDat %>% mutate(Pomo = ifelse(dist_pomo <= 0, "Inside", "Outside"))
mDat %>% group_by(Pomo) %>% 
  summarise(change_log_mean = abs(mean(change_log)), 
            change_log_sd = abs(sd(change_log)), 
            change_mean = 10^change_log_mean, 
            change_sd = 10^change_log_sd)

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

##### GET RELEVANT PORTS #####

#### By hours
pd_testDat <- left_join(dcp_plot, pd_complete2, by = "port_per_trip") 
pd_testDat %>% filter(nation == "HRV") %>% .$Study_area %>% quantile(., na.rm = TRUE)
pd_testDat %>% filter(nation == "ITA") %>% .$Study_area %>% quantile(., na.rm = TRUE)
pd_testDat %>% .$Study_area %>% quantile(., na.rm = TRUE)

## Abbreviate names
pd_testDat <- pd_testDat %>% mutate(port_abbrev = substr(port_per_trip, start = 1, stop = 3))
pd_testDat <- pd_testDat %>% filter(Study_area > 0)
pd_testDat %>% .$Study_area %>% quantile(., seq(0,1,0.1), na.rm = TRUE)

## Remove ports below 5th quantile of study hours (1.9 hours)
min_study_area <- pd_testDat %>% .$Study_area %>% quantile(., 0.5, na.rm = TRUE) %>% as.numeric()
min_study_area
pd_testDat %>% filter(Study_area > min_study_area)

## Remove ports below 75th quantile (for mapping)
min_total_hrs <- pd_testDat %>% .$Study_area %>% quantile(., 0.75, na.rm = TRUE)

## 0.75 quantile for both nations
pd_testDat_sub <- pd_testDat %>% filter(Study_area > min_total_hrs)
pd_ita <- pd_testDat_sub %>% filter(nation == "ITA") 
pd_ita_anc <- pd_testDat_sub %>% filter(port_per_trip == "ANCONA")
pd_hrv <- pd_testDat_sub %>% filter(nation == "HRV") 
pd_hrv_kom <- pd_hrv %>% filter(port_per_trip == "KOMIZA")
pd_hrv <- pd_hrv %>% filter(port_per_trip != "KOMIZA") # Remove Komiza

##### PLOT MAP OF EFFORT - with ports #####
test_dat <- mDat
## Get axis limits for log change scale
change_log_abs_value <- round(max(abs(range(test_dat$change_log))), 1) + 0.1

basemap <- get_base_map_general(test_dat, range_extension = 0.25, my_color = "black")

basemap <- get_base_map_general(test_dat, range_unequal = TRUE,
                                long_plus = 0.1, long_minus = 0.05,
                                lati_plus = 0, lati_minus = 0.3,
                                my_color = "black")

theme_set(theme_bw(base_size = 14) + 
            theme(strip.background = element_blank()))
text_size <- 4

map_change_port <- basemap + 
  geom_raster(data = test_dat, aes(SI_LONG, SI_LATI, fill = change_log)) + 
  scale_fill_distiller(palette = "RdYlBu", type = "div", direction = -1,
                       limits = c(-change_log_abs_value, change_log_abs_value)) +
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = -50, size = 0.3, alpha = 0.2) + 
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = -100, size = 0.4, alpha = 0.4) + 
  geom_contour(data = adria_df, aes(SI_LONG, SI_LATI, z = z), color = "black", 
               breaks = -200, size = 0.5, alpha = 0.6) + 
  geom_path(data = hull_coords, aes(long, lat), alpha = 0.8) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.8) + 
  geom_point(aes(SI_LONG, SI_LATI, fill = change_log, size = Adriatic/1000),
             data = pd_testDat_sub, alpha = 1, color = "black", pch = 21) +
  geom_point(aes(SI_LONG, SI_LATI, fill = change_log, size = Adriatic/1000),
             data = pd_hrv_kom, alpha = 1, color = "black", pch = 21) +
  scale_size(range = c(2,5), breaks = c(1, 10, 30, 60), name = "Hours\n(x 1000)") + 
  # geom_text(aes(SI_LONG, SI_LATI, label = port_abbrev),
  #           data = pd_hrv, alpha = 1, color = "black",
  #           check_overlap = FALSE, size = text_size, nudge_x = 0.11, nudge_y = 0.03) +
  # geom_text(aes(SI_LONG, SI_LATI, label = port_abbrev),
  #           data = pd_ita, alpha = 1, color = "black",
  #           check_overlap = FALSE, size = text_size, nudge_x = -0.16, nudge_y = -0.01) +
  # geom_text(aes(SI_LONG, SI_LATI, label = port_abbrev),
  #           data = pd_ita_anc, alpha = 1, color = "black",
  #           check_overlap = FALSE, size = text_size, nudge_x = 0.0, nudge_y = -0.1) +
  # geom_text(aes(SI_LONG, SI_LATI, label = port_abbrev),
  #           data = pd_hrv_kom, alpha = 1, color = "black",
  #           check_overlap = FALSE, size = text_size, nudge_x = -0.1, nudge_y = -0.07) +
  scale_bar(lon = 13.45, lat = 42.0, 
            distance_lon = 25, distance_lat = 5, distance_legend = 12, 
            orientation = FALSE, arrow_length = 25, arrow_distance = 10, 
            arrow_north_size = 0, 
            dist_unit = "km", legend_size = 0, 
            rec_colour = "black", rec2_colour = "black", legend_colour = "black") +
  annotate("text", x = 13.5, y = 42.5, label = "Italy", size = 6) + 
  annotate("text", x = 16.15, y = 44.0, label = "Croatia", size = 6) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.box.margin=margin(rep(-15, 4)), 
        legend.background = element_rect(fill = "transparent")) +
  theme(legend.position = "bottom") + 
  guides(size = guide_legend(order = 2, title.position = "top", 
                             label.position = "bottom", label.hjust = 0.5, 
                             title = "Hours (\u00D71000)"), 
         fill = guide_colorbar(order = 1, title.position = "top", 
                               title = "Log change (hours)")) + 
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12))

ggsave("ms_1/ms_1_figs/Fig2a.jpg", height = 4.25, width = 4.5, dpi = 600)

##### PLOT CHANGE BOXPLOTS - GRID CELLS #####
theme_set(theme_bw(base_size = 16) + 
            theme(strip.background = element_blank()))

change_boxplot_cell <- test_dat %>% 
  ggplot(aes("a", change_log)) + 
  theme(panel.grid = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) + 
  geom_boxplot(fill = "gray", alpha = 1, outlier.shape = 1, outlier.size = 0.5, 
               outlier.color = "gray") + 
  scale_y_continuous(limits = c(-change_log_abs_value, change_log_abs_value)) + 
  ylab("Effort change (log)") + 
  # ggtitle("Cells") + 
  # theme(plot.title = element_text(size = 10)) + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("ms_1/ms_1_figs/Fig2b.jpg", height = 2, width = 1, dpi = 600)

##### PLOT CHANGE BUBBLEPLOT - PORTS #####

## Study area ports with change log for entire adriatic
dcp_plot_adria
## Subset the mapped ports
dcp_plot_adria_sub <- dcp_plot_adria[dcp_plot_adria$port_per_trip %in% pd_testDat_sub$port_per_trip, ]
## Link the two 
dcp_plot_adria <- dcp_plot_adria_sub %>% 
  mutate(mapped = "mapped") %>% select(port_per_trip, mapped) %>% 
  left_join(dcp_plot_adria, ., by = "port_per_trip") 

dcp_plot_adria <- dcp_plot_adria %>% 
  mutate(mapped = ifelse(is.na(mapped), "not_mapped", mapped))

#dcp_plot %>% 
myseed = 101
set.seed(myseed)

dcp_plot_adria %>% 
  ggplot(aes("a", change_log, size = Study_area, fill = mapped)) + 
  scale_size(range = c(1,4), breaks = c(1, 10, 30, 60), name = "Hours\n(x 1000)") + 
  theme(panel.grid = element_blank()) + 
  geom_jitter(alpha = 0.75, shape = 21, height = 0) + 
  ylab("Effort change (log)") + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) + 
  stat_summary(fun.y = "median", fun.ymin = "median", fun.ymax = "median",
               size = 0.35, width = 0.75, geom = "crossbar", aes(fill = NULL)) +
  scale_fill_manual(values = c("gray", "white"))

ggsave("ms_1/ms_1_figs/Fig2c.jpg", height = 2, width = 1, dpi = 600)
