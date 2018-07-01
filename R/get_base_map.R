################################################################################
##' @title Get a simple base map
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-07-11
##' @log 
################################################################################

library(mapdata)

get_base_map <- function(vmsDat){
  
  # Get latitude and longitude range limits
  rangeLat <- with(vmsDat, range(SI_LATI) + c(-0.25, 0.25))
  rangeLong <- with(vmsDat, range(SI_LONG) + c(-0.25, 0.25))
  
  # Get basemap
  map1_data <- data.frame(map('worldHires', 
                              xlim = rangeLong, 
                              ylim = rangeLat, plot = FALSE)[c('x', 'y')])
  
  map1_data <- map1_data %>% rename(SI_LONG = x, SI_LATI = y)
  
  map1 <- ggplot(map1_data, aes(SI_LONG, SI_LATI)) + 
    geom_path(size = 0.25) + coord_fixed() + 
    labs(x = "Longitude", y = "Latitude") + 
    scale_x_continuous(limits = rangeLong) + 
    scale_y_continuous(limits = rangeLat)
  
  # Return base map
  map1 
}


## 
get_base_map_poly <- function(vmsDat){
  
  # Get latitude and longitude range limits
  rangeLat <- with(vmsDat, range(SI_LATI) + c(-0.25, 0.25))
  rangeLong <- with(vmsDat, range(SI_LONG) + c(-0.25, 0.25))
  
  ## Use fortify
  coast_map <- fortify(map("worldHires", fill = TRUE, 
                           xlim = rangeLong, ylim = rangeLat, 
                           plot = FALSE)) 
  
  map1 <- ggplot(coast_map, aes(long, lat)) + 
    geom_map(map = coast_map, aes(map_id = region), 
             color = "black", fill = "lightgray", size = 0.25) + 
    coord_fixed() + 
    labs(x = "Longitude", y = "Latitude") + 
    scale_x_continuous(limits = rangeLong) + 
    scale_y_continuous(limits = rangeLat)
  
  # Return base map
  map1 
}

## 
get_base_map_general <- function(x, latitude = "SI_LATI", longitude = "SI_LONG", 
                                 range_extension = 0.25, my_color = "gray", my_fill = "gray", 
                                 range_unequal = FALSE, lati_plus = 0, lati_minus = 0, 
                                 long_plus = 0, long_minus = 0){
  
  SI_LONG = x[, longitude]
  SI_LATI = x[, latitude]
  
  # Get latitude and longitude range limits
  if(range_unequal == FALSE){
    rangeLat <- range(SI_LATI) + c(-range_extension, range_extension)
    rangeLong <- range(SI_LONG) + c(-range_extension, range_extension)
  }

  
  if(range_unequal == TRUE){
    # Get latitude and longitude range limits
    rangeLat <- range(SI_LATI) + c(-lati_minus, lati_plus)
    rangeLong <- range(SI_LONG) + c(-long_minus, long_plus)
  }
  
  ## Use fortify
  coast_map <- fortify(map("worldHires", fill = TRUE, 
                           xlim = rangeLong, ylim = rangeLat, 
                           plot = FALSE)) 
  
  map1 <- ggplot(coast_map, aes(long, lat)) + 
    geom_map(map = coast_map, aes(map_id = region), 
             color = my_color, fill = my_fill, size = 0.25) + 
    coord_fixed() + 
    labs(x = "Longitude", y = "Latitude") + 
    scale_x_continuous(limits = rangeLong) + 
    scale_y_continuous(limits = rangeLat) + 
    theme(panel.grid = element_blank())
  
  # Return base map
  map1 
}


