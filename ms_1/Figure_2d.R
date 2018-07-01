################################################################################
##' @title Figure 2d: plot effort change for vessels
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(tidyr)
library(ggplot2)

## Load data
load("workspace/ais_summary_data_revised/effort_scale_data.RData")

## Effort change functions
source("R/effort_change_functions.R")

##### RELABEL VES_CI #####
mutate_label <- . %>% 
  mutate(ves_ci_label = ifelse(ves_ci == "Control", "Control vessels", ves_ci), 
         ves_ci_label = ifelse(ves_ci_label == "Impacted", "Impacted vessels", ves_ci_label), 
         ves_ci_label = ifelse(ves_ci_label == "Other", "Other vessels", ves_ci_label))

dat_vessel_w <- effort_scale_data[[1]] %>% mutate_label %>% 
  filter(effort_type == "effort_hrs") %>% 
  filter(before > 0 | after > 0)

dat_port_w <- effort_scale_data[[2]] %>% mutate_label %>% 
  filter(effort_type == "effort_hrs") %>% 
  filter(before > 0 | after > 0)

dat_vesci <- effort_scale_data[[3]] %>% mutate_label

dat_vessel_total <- effort_scale_data[[4]] %>% mutate_label %>% 
  filter(effort_type == "effort_hrs") %>% 
  filter(before > 0 | after > 0) %>% 
  mutate(total_hrs = before + after)

quantile(dat_vessel_total$total_hrs)

dat_port_total <- effort_scale_data[[5]] %>% mutate_label %>% 
  filter(effort_type == "effort_hrs") %>%  
  filter(before > 0 | after > 0) %>% 
  mutate(total_hrs = before + after)
quantile(dat_port_total$total_hrs)

## Remove venezia (a clear mistake, only 0.25 total hours)
dat_port_total <- dat_port_total %>% filter(total_hrs > 1)

dat_vesci_total <- effort_scale_data[[6]] 

##### PUBLICATION FIGURES - EFFORT CHANGE AT PORT SCALE #####

dat_vessel_total

dat_vessel_total <- get_effort_change(dat_vessel_total, add_value = 1)
summary(dat_vessel_total)

myseed = 101
set.seed(myseed)

vessel_label_text <- data.frame(ves_ci = c("Control", "Impacted"), 
                                change_log = c(0.25, -0.3), 
                                text1 = c("con", "imp"))

#1f78b4
#33a02c

dat_vessel_total %>% arrange(change_log)
summary(dat_vessel_total$change_log)

dat_vessel_total %>% group_by(ves_ci) %>% 
  summarise(change_log_mean = abs(mean(change_log)), 
            change_log_sd = abs(sd(change_log)), 
            change_mean = 10^change_log_mean, 
            change_sd = 10^change_log_sd)

theme_set(theme_bw(base_size = 16) + 
            theme(strip.background = element_blank()))

dat_vessel_total %>% 
  ggplot(aes(ves_ci, change_log, fill = ves_ci)) + 
  theme(panel.grid = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) + 
  geom_boxplot(alpha = 1, outlier.shape = 1, outlier.size = 1, outlier.color = "black", 
               notch = FALSE) +
  scale_y_continuous(breaks = seq(-0.4, 0.6, by = 0.4)) +
  coord_cartesian(ylim = c(-0.4, 0.6)) +
  ylab("Effort change (log)") + 
  theme(legend.position = "none") + 
  theme(axis.title.y = element_blank()) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  #ggtitle("Vessels") + 
  theme(plot.title = element_text(size = 10)) + 
  geom_text(aes(ves_ci, change_log, label = text1, color = ves_ci), 
            data = vessel_label_text, size = 6, hjust = c(0.4, 0.6), vjust = 1, 
            show.legend = FALSE, fontface = "bold") + 
  scale_fill_manual(values = c("gray" ,"#1b9e77")) + 
  scale_color_manual(values = c("gray50" ,"#1b9e77"))

ggsave("ms_1/ms_1_figs/Fig2d.jpg", height = 2, width = 1.5, dpi = 600)
