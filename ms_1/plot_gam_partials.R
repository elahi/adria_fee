################################################################################
##' @title Plot partial effects from bam - aggregate effort hours
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

source("model_effort_ais/4_get_model_predictions.R")

ggDat <- new_df2 

## Rename x labels
unique(ggDat$xlab)
ggDat$xlab2 <- factor(ggDat$xlab, 
                      labels = c("Depth (m)", "Distance to\nJabuka/Pomo border (km)", 
                                 "Distance to\nshore (km)", "Habitat quality\n(nursery persistence)", 
                                 "Latitude", "Longitude","Bottom slope (degrees)"))
## Remove lat and long
ggDat <- ggDat %>% filter(xlab != "SI_LATI_z" & xlab != "SI_LONG_z")

## Remove depth
ggDat <- ggDat %>% filter(xlab != "depth_z")

## Label reserve_timing
levels(ggDat$reserve_timing)
ggDat$reserve_timing <- factor(ggDat$reserve_timing, labels = c("Before closure", "After closure"))

##### PLOT - BACKTRANSFORMED #####

p1_label_text <- ggDat %>% group_by(xlab2) %>% 
  summarise(x_pos = min(x), 
            y_pos = max(exp(uci)) * 0.99) %>% 
  mutate(text1 = c("A", "B", "C", "D"), 
         text2 = c("(a)", "(b)", "(c)", "(d)"))

ggDat %>% 
  ggplot(aes(x, exp(Estimate), linetype = reserve_timing)) + 
  geom_vline(aes(xintercept = 0, linetype = NULL), color = "gray", linetype = "dashed",
             data = subset(ggDat, xlab == "dist_pomo_z")) +
  geom_ribbon(aes(x = x, ymin = exp(lci), ymax = exp(uci), linetype = NULL, 
                  group = reserve_timing), 
              fill = "gray", alpha = 0.5) + 
  geom_line() + 
  ylab("Partial contribution to fishing effort (hours)") + 
  theme(panel.grid = element_blank()) + 
  xlab("") + 
  facet_wrap(~ xlab2, scales = "free", strip.position = "bottom") + 
  theme(strip.placement = "outside") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = c(0.35, 0.9)) + 
  geom_text(aes(x_pos, y_pos, label = text2, linetype = NULL), 
            data = p1_label_text, size = 4, hjust = 0.25, vjust = 0.75, 
            show.legend = FALSE) 

ggsave("ms_1/ms_1_figs/plot_gam_partials_free_hrs.png", height = 7, width = 7, dpi = 300)


