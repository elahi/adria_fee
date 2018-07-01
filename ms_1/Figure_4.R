################################################################################
##' @title Figure 4: plot effort hours by nursery species
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

##### SOURCE DATA, FUNCTIONS #####

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

source("model_effort_ais/model_effort_ais_R/load_d_change.R")
d_change <- load_d_change(df_name = "aggregate") %>% tbl_df()
summary(d_change)

scaleFUN <- function(x) sprintf("%.0f", x)

##### EFFORT HOURS - ALL FIVE SPECIES #####

names(d_change)
summary(d_change)
dcl <- d_change %>% 
  select(effort_type, SI_LATI, SI_LONG, Broadtail_shortfin_squid:Norway_lobster, 
         before, after) %>% 
  gather(key = species, value = persistence, Broadtail_shortfin_squid:Norway_lobster) %>% 
  gather(key = reserve_timing, value = effort, before:after) %>% 
  mutate(persistence = as.character(persistence))

effort_stats <- . %>% summarise(mean = mean(effort), 
                                med = median(effort), 
                                sd = sd(effort), 
                                n = n(), 
                                se = sd/sqrt(n), 
                                CI = qt(0.975, df = n-1) * se, 
                                UCI = mean + CI, 
                                LCI = mean - CI)

summary_effort <- dcl %>% 
  filter(!is.na(persistence)) %>%
  group_by(effort_type, species, persistence, reserve_timing) %>% 
  effort_stats() %>%
  ungroup() %>% 
  mutate(reserve_timing = factor(reserve_timing, levels = c("before", "after"), 
                                 labels = c("Before\nclosure", "After\nclosure")), 
         species = factor(species, labels = c("Broadtail shortfin squid", 
                                              "Deepwater rose shrimp", 
                                              "European hake", 
                                              "Horned octopus", 
                                              "Norway lobster")))

se2 <- summary_effort %>% complete(effort_type, persistence, species, reserve_timing)

## 3 columns
p1_label_text <- summary_effort %>% 
  filter(effort_type == "effort_hrs") %>%
  filter(persistence != "0") %>% 
  group_by(species) %>% 
  summarise(x_pos = 5, 
            y_pos = max(UCI) * 0.95) %>% 
  mutate(text1 = c("A", "B", "C", "D", "E"), 
         text2 = c("(a)", "(b)", "(c)", "(d)", "(e)"), 
         x_pos = c(5,4,5,5,5))

pos_dodge_width = 0.6

se2 %>% 
  filter(effort_type == "effort_hrs") %>%
  filter(persistence != "0") %>% 
  ggplot(aes(persistence, mean, fill = reserve_timing)) + 
  geom_errorbar(aes(ymin = mean - CI, ymax = mean + CI), width = pos_dodge_width/1.5, 
                position = position_dodge(width = pos_dodge_width)) + 
  geom_point(aes(size = n), position = position_dodge(width = pos_dodge_width), 
             alpha = 1, pch = 21) + 
  scale_size(range = c(0.5, 2.5), guide = guide_legend(title = "Cells (n)")) + 
  theme(legend.position = c(0.75, 0.1), 
        legend.box = "horizontal") +
  facet_wrap(~ species, scales = "free", ncol = 2) + 
  ylab("Fishing effort (hours)") + 
  xlab("Nursery persistence") + 
  theme(panel.grid = element_blank()) + 
  scale_y_continuous(labels = scaleFUN) + 
  geom_text(aes(0.75, 9, label = text2, fill = NULL), 
            data = p1_label_text, size = 4, hjust = 0.25, vjust = 0.25, 
            show.legend = FALSE) +
  coord_cartesian(ylim = c(8, 24)) + 
  scale_fill_viridis_d(begin = 0, end = 0.75, option = "viridis", 
                        guide = guide_legend(title = NULL, label.position = "top"))

ggsave("ms_1/ms_1_figs/Fig4.jpg", height = 5, width = 4.5, dpi = 600)

