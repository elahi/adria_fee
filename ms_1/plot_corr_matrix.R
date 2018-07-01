################################################################################
##' @title Plot correlation matrix of covariates
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

source("model_effort_ais/model_effort_ais_R/load_augmented_data.R")

## Variance inflation functions
source("model_effort_ais/model_effort_ais_R/HighstatLibV6.R")

library(broom)
library(ggplot2)
library(ggcorrplot)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

##### PAIRS PLOT #####
# Get correlation plot
m_aug %>% 
  filter(reserve_timing == "before") %>% 
  select(dist_shore, dist_pomo, depth, slope, persist_sum) %>% Mypairs()

##### VARIANCE INFLATION FACTORS #####
# With bathymetry
m_aug %>%
  filter(reserve_timing == "before") %>% 
  select(dist_shore, dist_pomo, depth, slope, persist_sum) %>% corvif()

# Without bathymetry
m_aug %>%
  filter(reserve_timing == "before") %>% 
  select(dist_shore, dist_pomo, slope, persist_sum) %>% corvif()

##### PLOT CORRELATION MATRIX #####

res <- m_aug %>%
  filter(reserve_timing == "before") %>% 
  select(dist_shore, dist_pomo, depth, slope, persist_sum) %>% 
  rename(`Distance to shore` = dist_shore, 
         `Distance to Pomo` = dist_pomo, 
         Depth = depth, 
         `Bottom slope` = slope, 
         `Habitat quality` = persist_sum) %>% 
  cor()

p_corr <- ggcorrplot(res, hc.order = FALSE, type = "lower",
           lab = TRUE, show.legend = T, 
           lab_size = 3, legend.title = "", 
           tl.cex = 8) 
p_corr

ggsave("ms_1/ms_1_figs/plot_corr_matrix.png", height = 3.5, width = 3.5)
