################################################################################
##' @title Figure 3: plot model residuals
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##### LOAD DATA, PACKAGES #####

## For mapping
source("R/get_base_map.R")
source("R/identify_poly_pings.R")
source("R/identify_adriatic_points.R")
source("R/capitalization_functions.R")

source("R/map_adriatic_reserves.R")
pomo_list <- load_pomo()
pomo <- pomo_list[[1]]
pomo_df <- pomo_list[[2]]

library(marmap)
library(legendMap)
library(ggplot2)
library(cowplot)
library(mgcv)

## Load final bam model
load(file = "workspace/model_data_revised/bam_2.RData")

# model to check
m_check <- bam_2
summary(m_check)
m_check

## Adriatic port data
pd_complete2 <- read.csv("workspace/pd_complete2.csv") %>% select(-X)
## Change capitalization
port <- pd_complete2$port_per_trip
port <- tolower(as.character(port))
port <- sapply(port, simpleCap)
pd_complete2 <- pd_complete2 %>% 
  mutate(port_text = ifelse(is.na(port_per_trip), NA, port))
unique(pd_complete2$port_text)

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

##### PLOT SETTINGS #####

theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank(), 
                  axis.text.y = element_text(angle = 90, hjust = 0.5)))

##### PLOT FISHING THE LINE RESIDUALS #####

min_pomo_distance <- m_augL %>% 
  filter(variable_p == "Distance_to_Pomo") %>%
  summarise(min_pomo = min(value_p)) %>% .$min_pomo

m_aug2 <- m_aug %>% filter(dist_pomo <= -min_pomo_distance) %>% 
  mutate(reserve_timing = factor(reserve_timing, 
                                 levels = c("before", "after"), labels = c("Before closure", "After closure")))

## Get axis limits for resid scale
summary(m_aug2$.resid)
quantile(m_aug2$.resid, probs = seq(0.9, 1, 0.01))

max_resid <- quantile(m_aug2$.resid, probs = .999)
max_resid <- -min(m_aug2$.resid) + 0.1

resid_abs_value <- m_aug2 %>% 
  filter(.resid < max_resid) %>% 
  summarise(v = round(max(abs(range(.resid))), 1) + 0.1) %>% unlist(use.names = F)

m_aug2 <- m_aug2 %>% mutate(.resid2 = ifelse(.resid >= max_resid, NA, .resid))

p1_label_text <- data.frame(reserve_timing = c("Before closure", "After closure"), 
                            x_pos = c(14, 14), y_pos = c(6.5, 6.5),
                            text1 = c("A", "B"), 
                            text2 = c("(a)", "(b)"))

## Y axis label
ylab1 <- expression(atop("Deviation from expected", '(log'[e]*'[hours])'))
ylab1 <- expression('Residual log'[e]*'(hours)')

p1 <- m_aug2 %>% 
  ggplot(aes(dist_pomo, .resid, color = .resid2)) + 
  facet_wrap(~ reserve_timing) + 
  geom_point(alpha = 1) + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") + 
  geom_vline(xintercept = 0, color = "gray", linetype = "dashed") + 
  geom_smooth(color = "black", fill = "black", size = 0.5, alpha = 1) + 
  theme(panel.grid = element_blank()) + 
  xlab("Distance to Jabuka/Pomo border (km)") + 
  ylab(ylab1) + 
  scale_color_distiller(palette = "RdYlBu", type = "div", direction = -1, 
                        limits = c(-resid_abs_value, resid_abs_value)) + 
  geom_point(data = m_aug2[m_aug2$.resid >= max_resid, ], color = "black") + 
  theme(legend.position = "none") + 
  theme(legend.title = element_blank()) + 
  geom_text(aes(x_pos, y_pos, label = text2, color = NULL), 
            data = p1_label_text, size = 5, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) 
p1

##### MAP FISHING THE LINE RESIDUALS #####

## Map of residuals
basemap <- get_base_map_general(m_aug2, range_extension = 0.12, my_color = "black")

theme_blank <- function(...) {
  ret <- theme_bw(...)
  ret$line <- element_blank()
  #ret$rect <- element_blank()
  ret$strip.text <- element_blank()
  ret$axis.text <- element_blank()
  ret$plot.title <- element_blank()
  ret$axis.title <- element_blank()
  #ret$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
  ret
}


pd_testDat <- pd_complete2 %>% mutate(port_abbrev = substr(port_per_trip, start = 1, stop = 3))
pd_testDat <- pd_testDat %>% filter(port_per_trip == "PESCARA" | port_per_trip == "TRIBUNJ") 
pd_pescara <- pd_testDat %>% filter(port_per_trip == "PESCARA") 
pd_tribunj <- pd_testDat %>% filter(port_per_trip == "TRIBUNJ") 

range(m_aug2$SI_LONG)
range(m_aug2$SI_LATI)
p2_label_text <- data.frame(reserve_timing = c("Before closure", "After closure"), 
                            x_pos = c(15.65, 15.65), y_pos = 42.35,
                            text1 = c("C", "D"), 
                            text2 = c("(c)", "(d)"))

ylab2 <- expression('Deviation from expected (log'[e]*'[hours])')

p2 <- basemap + 
  geom_raster(data = m_aug2, 
              aes(SI_LONG, SI_LATI, fill = .resid2)) + 
  scale_fill_distiller(palette = "RdYlBu", type = "div", direction = -1, 
                       limits = c(-resid_abs_value, resid_abs_value), 
                       name = "Residual", na.value = "black", 
                       breaks = c(-2, 0, 2)) + 
  geom_path(data = pomo_df, aes(long, lat), color = "black", size = 0.5, alpha = 0.75) +
  geom_point(aes(SI_LONG, SI_LATI), data = pd_testDat,
             size = 3, alpha = 1, color = "black", fill = "white", shape = 21) +
  facet_wrap(~ reserve_timing) +
  geom_text(aes(SI_LONG, SI_LATI, label = port_abbrev),
            data = pd_testDat, alpha = 1, color = "black",
            check_overlap = FALSE, size = 3, nudge_x = 0.0, nudge_y = -0.1) +
  geom_text(aes(x_pos, y_pos, label = text2, color = NULL),
            data = p2_label_text, size = 5, hjust = 0, vjust = 0,
            show.legend = FALSE) +
  theme(legend.position = c(0.51, 0.99), legend.justification = c(0, 1)) +
  theme(legend.background = element_blank())  +
  theme(legend.title = element_text(size = 10)) + 
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 4, title.position = "top"))

p2

##### MAKE FOUR PANEL PLOT #####
plot4 <- plot_grid(p1, p2, ncol = 1)
save_plot("ms_1/ms_1_figs/Figure_3.png", plot4, 
          nrow = 2, ncol = 1, base_height = 2.5, base_width = 4.5, dpi = 300)

##### EXAMINE OUTLIER #####

## Examine the outlier
outlier <- m_aug %>% filter(.resid >= 5)
outlier_lat <- outlier$SI_LATI
outlier_long <- outlier$SI_LONG
extra_value <- 0.10
# 242 cells within 0.1 degree (11 km of latitude; 8 km of longitude)
outlier_dat <- m_aug %>% 
  filter(SI_LONG > outlier_long - extra_value & SI_LONG < outlier_long + extra_value) %>% 
  filter(SI_LATI > outlier_lat - extra_value & SI_LATI < outlier_lat + extra_value) 
summary(outlier_dat)
