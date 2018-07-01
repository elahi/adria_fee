
## For mapping
source("R/get_base_map.R")
source("R/identify_poly_pings.R")

source("R/map_adriatic_reserves.R")
pomo_list <- load_pomo()
pomo <- pomo_list[[1]]
pomo_df <- pomo_list[[2]]

library(broom)
library(ggplot2)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

## Load final bam model
load(file = "workspace/model_data_revised/bam_2.RData")

# model to check
m_check <- bam_2

# dependent variable (for plotting below)
dep_var <- "effort_plus"

## Basics
m_check
summary(m_check)

## Augment the dataset with fitted and residual values
m_aug <- test_dat %>% 
  mutate(.fitted = fitted(m_check), .resid = resid(m_check), 
         pomo = ifelse(dist_pomo >= 0, "outside_closure", "inside_closure"))

## Map of residuals
m_aug %>% 
  ggplot(aes(SI_LONG, SI_LATI)) + 
  geom_raster(aes(fill = .resid)) + 
  facet_wrap(~ reserve_timing)

## Density plot of hours
m_aug %>% 
  #filter(.fitted > 50) %>% 
  ggplot(aes(.fitted, fill = reserve_timing)) +
  geom_density(alpha = 0.5)

## Density plot of hours
m_aug %>% 
  #filter(effort_plus > 100) %>% 
  ggplot(aes(effort_plus, fill = reserve_timing)) +
  geom_density(alpha = 0.5)

## Observed vs fitted
m_aug %>% 
  ggplot(aes_string(".fitted", dep_var)) +
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  geom_smooth(method = "lm") + 
  xlab("Fitted effort (hours)") + 
  ylab("Observed effort (hours)")

ggsave("model_effort_ais/model_effort_ais_figs/bam_2_obs_fit.png",
       height = 3.5, width = 3.5)

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

## Plot residuals against relevant covariates
m_augL %>% 
  ggplot(aes(value_p, Residuals)) + 
  facet_wrap(~ variable_p, scales = "free_x", 
             strip.position = "bottom", ncol = 2) + 
  theme(strip.placement = "outside") + 
  geom_point(alpha = 0.2) + geom_smooth() + 
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") + 
  xlab("")

ggsave("model_effort_ais/model_effort_ais_figs/bam_2_resids.png",
       height = 7, width = 5)
