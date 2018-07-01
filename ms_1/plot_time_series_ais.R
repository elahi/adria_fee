################################################################################
##' @title Plot monthly time-series of fishing effort by nation
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##### LOAD PACKAGES, DATA #####

## Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## Load weekly time series of fishing hours
load("workspace/ais_summary_data_revised/ais_hrs_nation_monthly_study_area.RData")

## Pomo closure was established 25 July 2015
closure_date <- "2015-07-25"

##### PLOT TIME SERIES #####

effort_hrs_nation_monthly <- effort_hrs_nation_monthly %>% 
  mutate(ves_nation = factor(ves_nation, 
                             levels = c("ITA", "HRV"), 
                             labels = c("Italy", "Croatia")), 
         reserve = factor(reserve, labels = c("Inside Jabuka/Pomo closure", 
                                              "Outside Jabuka/Pomo closure")), 
         MONTH = month(SI_DATE), YEAR = year(SI_DATE), 
         SI_DATE = ymd(paste(YEAR, MONTH, 15, sep = "-")))

levels(effort_hrs_nation_monthly$ves_nation)

## get dates of biological stops in 2014 and 2015
## Pesaro to Bari
##' 2015: Pesaro to Bari, 16 August – 27 September (43 days)
##' 2014: Pesaro to Bari, 11 August – 21 September (42 days)

bio_stop_2014 <- data.frame(x_start = ymd("2014-08-11"), 
                            x_end = ymd("2014-09-21"))

bio_stop_2015 <- data.frame(x_start = ymd("2015-08-16"), 
                            x_end = ymd("2015-09-27"))

effort_hrs_nation_monthly %>% 
  ggplot(aes(SI_DATE, sum/1000, color = ves_nation)) + 
  geom_segment(aes(x = x_start, y = 0, xend = x_end, yend = 0, color = NULL), 
               data = bio_stop_2014, size = 2, alpha = 0.5, show.legend = F) + 
  geom_segment(aes(x = x_start, y = 0, xend = x_end, yend = 0, color = NULL), 
               data = bio_stop_2015, size = 2, alpha = 0.5, show.legend = F) + 
  geom_line() + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ reserve, scales = "free", ncol = 1) + 
  geom_vline(xintercept = as.numeric(ymd(closure_date)), 
             col = "black", linetype = "dashed") + 
  xlab("Year") + ylab("Fishing effort (hours x 1000)") + 
  geom_vline(xintercept = as.numeric(ymd(closure_date) - 365), 
             col = "gray", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ymd(closure_date) + 365), 
             col = "gray", linetype = "dashed") +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  theme(panel.grid.minor  = element_blank(), 
        legend.title = element_blank(), 
        legend.position = c(0.95, 1), 
        legend.justification = c(0.95, 1), 
        legend.background = element_blank(), 
        legend.key = element_blank()) 
  

ggsave("ms_1/ms_1_figs/plot_time_series.png", height = 5, width = 3.5)

