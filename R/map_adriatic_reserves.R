###############################################################################
##' @title Map Adriatic reserves
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2018-04-14
##' @log Add a log here
################################################################################

source("R/fortify_shapefile.R")

library(ggplot2)
theme_set(theme_bw(base_size = 12) +
            theme(panel.grid = element_blank()))
library(mapdata)
library(rgdal)
library(maptools)
library(dplyr)

### These are the ranges and resolution I used for VMS analysis
rangeLong <- c(12, 19)
rangeLat <- c(40, 46)
bin_lat_vector <- seq(40, 46, length.out = 361)
bin_long_vector <- seq(12, 19, length.out = 421)
bin_lat_vector # interval = 0.01667 decimal degrees = 1 decimal minute

##### RESTRICTED AREAS #####

### Temporary pomo closure (July 25 2015)

load_pomo <- function(){
  sp_poly <- readOGR(dsn = "shapefiles/restricted_areas/pomo", 
                  layer = "POMO_noTrawlArea")
  sp_poly_df <- fortify_shape(sp_poly)
  list1 <- list("pomo" = sp_poly, "pomo_df" = sp_poly_df)
  return(list1)
}
