################################################################################
##' @title Plot monthly time-series of fishing effort for suspect vessels
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##' In response to reviewer comments about the port of origin of vessels that were 
##' observed in the Jabuka-Pomo area during the closure period

##### LOAD PACKAGES, DATA #####

## Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## Functions
source("R/effort_change_functions.R")
source("R/capitalization_functions.R")

## Load monthly time series of fishing hours
load("workspace/ais_summary_data_revised/ais_hrs_ves_port_monthly_study_area_suspect.RData")
glimpse(ves_hrs_monthly_suspect)

## Pomo reserve was established 25 July 2015
closure_date <- "2015-07-25"

## Impacted vessels
source("ms_1/Figure_2d.R")
ves_ci <- dat_vessel_total %>% distinct(VE_REF, ves_ci)
ves_ci %>% count(ves_ci)

## Change capitalization
port <- ves_hrs_monthly_suspect$port_per_trip
port <- tolower(as.character(port))
port
unique(port)
port <- sapply(port, simpleCap)
port

ves_hrs_monthly_suspect <- ves_hrs_monthly_suspect %>% 
  rename(port_upper = port_per_trip) %>% 
  mutate(port_per_trip = ifelse(is.na(port_upper), NA, port))

unique(ves_hrs_monthly_suspect$port_upper)
unique(ves_hrs_monthly_suspect$port_per_trip)

##### SUMMARISE SUSPECTS #####

ves_port_suspects <- ves_hrs_monthly_suspect %>% 
  filter(reserve_timing == "after", reserve == "Inside_reserve") %>% 
  group_by(VE_REF, port_per_trip) %>%
  summarise(sum = sum(sum)) %>% 
  arrange(desc(sum)) %>% 
  ungroup()

ves_suspects <- ves_hrs_monthly_suspect %>% 
  filter(reserve_timing == "after", reserve == "Inside_reserve") %>% 
  group_by(VE_REF) %>%
  summarise(sum = sum(sum)) %>% 
  arrange(desc(sum)) %>% 
  ungroup()

##### SUMMARISE MONTHLY DATA BY VESSEL #####

ves_suspects <- ves_suspects %>% rename(sum_after_ves = sum) %>% 
  mutate(VE_REF = factor(VE_REF))

## Reorder by sum_after_ves
ves_suspects <- ves_suspects %>% 
  mutate(VE_REF = reorder(VE_REF, sum_after_ves, min))

## Summarise by vessel
by_vessel <- ves_hrs_monthly_suspect %>%
  group_by(VE_REF, ves_nation, reserve, reserve_timing, YEAR, MONTH, SI_DATE) %>%
  summarise(sum = sum(sum, na.rm = T)) %>%
  ungroup()

by_vessel <- by_vessel %>% mutate(ves_nation = factor(ves_nation, 
                                                  levels = c("ITA", "HRV"), 
                                                  labels = c("Italy", "Croatia")))

## Join with ves_ci
by_vessel <- ves_ci %>% 
  left_join(by_vessel, ., by = "VE_REF")

by_vessel %>% count(ves_ci)

by_vessel <- by_vessel %>% 
  mutate(ves_ci = ifelse(is.na(ves_ci), "Other", ves_ci))

by_vessel <- by_vessel %>% left_join(., ves_suspects, by = "VE_REF") %>% 
  mutate(VE_REF = factor(VE_REF, levels = ves_suspects$VE_REF))

levels(by_vessel$VE_REF)

##### PLOT MONTHLY DATA BY VESSEL #####

## Threshold for plotting individual vessels
my_threshold <- quantile(ves_suspects$sum_after_ves, probs = 0.75)

## Filter data to be plotted
by_ves_plot <- by_vessel %>%
  filter(sum_after_ves > my_threshold) %>% 
  filter(reserve == "Inside_reserve")

## Get the suspects to be plotted
ves_port_suspects_plot <- ves_port_suspects[ves_port_suspects$VE_REF %in% by_ves_plot$VE_REF, ]

## Concatenate port name and hours    
ves_port_suspects_plot <- ves_port_suspects_plot %>% 
  mutate(port_sum = paste(port_per_trip, " (", round(sum), ")", sep = ""), 
         VE_REF = reorder(factor(VE_REF), sum, max))

## Create separate dataframes for the hours per port (for geom_text)
vpsp1 <- ves_port_suspects_plot %>% 
  group_by(VE_REF) %>% slice(1) %>% ungroup() 
vpsp2 <- ves_port_suspects_plot %>% 
  group_by(VE_REF) %>% slice(2) %>% ungroup()
vpsp3 <- ves_port_suspects_plot %>% 
  group_by(VE_REF) %>% slice(3) %>% ungroup()

by_ves_plot %>% count(ves_ci)

## Get labels for facets
by_ves_plot %>% count(VE_REF)
by_ves_plot <- by_ves_plot %>% 
  mutate(VE_REF1 = as.integer(VE_REF)) %>% 
  mutate(VE_REF2 = letters[VE_REF1],
         VE_REF3 = paste("(", VE_REF2, ")", sep = ""))

facet_labels <- by_ves_plot %>% distinct(VE_REF, VE_REF1, VE_REF2, VE_REF3) %>% arrange(VE_REF1)

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

## Plot
by_ves_plot %>%
  ggplot(aes(SI_DATE, sum, group = VE_REF, color = ves_nation, shape = ves_ci)) +
  geom_hline(yintercept = 0, color = "black") + 
  facet_wrap(~ VE_REF, ncol = 2) + 
  geom_point(alpha = 1) +  geom_line(alpha = 1) +
  geom_vline(xintercept = as.numeric(ymd(closure_date)),
             col = "black", linetype = "dashed") +
  xlab("Year") + ylab("Fishing effort (hours)") +
  geom_vline(xintercept = as.numeric(ymd(closure_date) - 365),
             col = "gray", linetype = "dashed") +
  geom_vline(xintercept = as.numeric(ymd(closure_date) + 365),
             col = "gray", linetype = "dashed") + 
  geom_text(aes(ymd("2014-01-01"), 110, label = port_sum, color = NULL, shape = NULL), 
            data = vpsp2, hjust = 0, size = 2, fontface = "bold") + 
  # geom_text(aes(ymd("2014-01-01"), 70, label = port_sum, color = NULL, shape = NULL),
  #           data = vpsp3, hjust = 0, size = 2, fontface = "bold") +
  geom_text(aes(ymd("2014-01-01"), 150, label = port_sum, color = NULL, shape = NULL), 
              data = vpsp1, hjust = 0, size = 2, fontface = "bold") + 
  geom_text(aes(ymd("2016-06-01"), 150, label = VE_REF3, color = NULL, shape = NULL), 
            data = facet_labels, hjust = 0, size = 2.5, fontface = "bold") + 
  geom_line(alpha = 1) + 
  scale_color_discrete(name = "Nation") + 
  scale_shape_discrete(name = "Vessel")

ggsave("ms_1/ms_1_figs/plot_time_series_incursion_ves_after.png",
       height = 7, width = 4.5, dpi = 300)

## What percentage of the data am I plotting?
total_hrs_after <- sum(ves_suspects$sum_after_ves)

total_hrs_after_plot <- by_ves_plot %>%
  filter(reserve_timing == "after") %>% 
  summarise(sum_after_inside = sum(sum)) %>% unlist(use.names = F)

total_hrs_after_plot / total_hrs_after * 100

total_hrs_after_plot_top2 <- by_ves_plot %>%
  filter(reserve_timing == "after") %>% 
  filter(sum_after_ves > 100) %>%
  summarise(sum_after_inside = sum(sum)) %>% unlist(use.names = F)

total_hrs_after_plot_top2 / total_hrs_after * 100

## write ves_suspects file
write.csv(ves_suspects, "output/ves_suspects_df.csv")
