################################################################################
##' @title Plot spatial correlogram
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

source("model_effort_ais/model_effort_ais_R/load_augmented_data.R")
library(broom)
library(ggplot2)
library(cowplot)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))
library(ncf) # for correlograms

## Function to get spline correlogram per some subsetted sample (e.g., random subset, vessel, etc)
get_spline_correlog <- function(x, ...){
  
  # Get correlog object
  x_spline_corr <- spline.correlog(x$SI_LONG, x$SI_LATI, x$.resid, 
                                   resamp = 0, latlon = TRUE, xmax = ...)
  
  # Create new dataframe
  pred_x <- x_spline_corr$real$predicted$x
  pred_y <- x_spline_corr$real$predicted$y
  corr_df <- data.frame(pred_x, pred_y)
  
}

##### APPLY FUNCTION #####
m_aug_bef <- m_aug %>% filter(reserve_timing == "before")
m_aug_aft <- m_aug %>% filter(reserve_timing == "after")

## Entire dataset [before]
system.time(corr_df_bef <- get_spline_correlog(m_aug_bef, xmax = 50)) # 350 seconds
summary.spline.correlog(corr_df_bef)
## Entire dataset [after]
system.time(corr_df_aft <- get_spline_correlog(m_aug_aft, xmax = 50)) # 340 seconds

# write.csv(corr_df_bef, "workspace/corr_df_bef.csv")
# write.csv(corr_df_aft, "workspace/corr_df_aft.csv")

###### SIMPLE PLOT OF BEFORE AND AFTER ######

corr_df_bef <- read.csv("workspace/corr_df_bef.csv")
corr_df_aft <- read.csv("workspace/corr_df_aft.csv")

png("ms_1/ms_1_figs/plot_correlogram.png", height = 5, width = 5, units = "in", res = 300)
with(corr_df_bef, plot(pred_x, pred_y, type = "l", col = "red", 
                       xlab = "Distance (km)", ylab = "Correlation", ylim = c(-1,1)))
with(corr_df_aft, points(pred_x, pred_y, type = "l", col = "blue"))
abline(h = 0, lty = 2)
legend(10, 1, c("Before closure", "After closure"), lty = 1, col = c("red", "blue"))
dev.off()
