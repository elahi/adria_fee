################################################################################
##' @title Plot interval threshold
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

library(broom)
library(ggplot2)
library(cowplot)
theme_set(theme_bw(base_size = 12) + 
            theme(strip.background = element_blank()))

# For reverse log scale
library(scales)

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

intv_df <- read.csv("workspace/ais_interval_sensitivity.csv")

intv_seq_axis <- c(2,4,8,12,24,48,120,1000,10000)
intv_df_max = intv_df %>% filter(X > 18)
intv_df_long <- gather(intv_df, key = facet_variable, 
                       value = value, n_trips:trips_per_120 )

ais_intv_99 = 991/60

ais_pA <- intv_df %>% 
  ggplot(aes(intvMax_hrs, trip_hrs_max)) + 
  geom_vline(xintercept = ais_intv_99, 
             linetype = "dashed", color = "gray70", size = 1.1) + 
  geom_line() + geom_point(alpha = 0.7) + 
  scale_x_continuous(trans=reverselog_trans(10), breaks = intv_seq_axis) + 
  scale_y_log10(breaks = c(600, 1200, 2400, 4800, 9600, 9600*2)) + 
  xlab("Interval threshold (hours)") + 
  ylab("Maximum trip duration (hours)") + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  geom_point(data = intv_df_max, size = 2, color = "red") + 
  annotate(geom = "text", x = 2, y = 19200, label = "(a)", size = 6)

ais_pB <- intv_df %>% 
  ggplot(aes(intvMax_hrs, trips_per_120)) + 
  geom_vline(xintercept = ais_intv_99, 
             linetype = "dashed", color = "gray70", size = 1.1) + 
  geom_line() + geom_point(alpha = 0.7) + 
  scale_x_continuous(trans=reverselog_trans(10), breaks = intv_seq_axis) + 
  scale_y_continuous(breaks = seq(90, 100, by = 2)) + 
  xlab("Interval threshold (hours)") + 
  ylab("Trips < 120 hours (%)") + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) + 
  geom_point(data = intv_df_max, size = 2, color = "red") + 
  annotate(geom = "text", x = 2, y = 90.8, label = "(b)", size = 6)

## Plot 2panels
plot_grid(ais_pA, ais_pB, ncol = 1)

ggsave("ms_1/ms_1_figs/plot_interval_threshold.png", height = 7, width = 7)
