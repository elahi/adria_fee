################################################################################
##' @title Identify points in/out of polygon
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-09-29
##' 
##' @log Add a log here
##' 
################################################################################

##' Functions takes as input:
##' vmsDat = dataset formatted to vmstools specifications 
##' polygon_df = the desired polygon to test for points in vs out

library(rgdal)
library(sp)

#vmsDat = vdat %>% slice(1:500000)
#polygon_df = pomo

identify_poly_pings <- function(vmsDat, polygon_df) {
  
  # Turn dataframe to SpatialPointsDataFrame, match projection
  adrpunti <- vmsDat
  coordinates(adrpunti) <- c("SI_LONG", "SI_LATI")
  proj4string(adrpunti) <- proj4string(polygon_df) 
  
  # Obtain points inside the polygon 
  pt_in_poly <- sp::over(adrpunti, as(polygon_df, "SpatialPolygons"))
  
  # Make dataframe where TRUE means the point was outside of the polygon (i.e., buffer)
  outside_poly_df <- data.frame(outside_poly = is.na(pt_in_poly))
  
  # Join to vms dataframe, 
  vmsDat2 <- cbind(vmsDat, outside_poly_df)
  
  return(vmsDat2)
  
}


identify_poly_bins <- function(dat_grid, polygon_df) {
  
  # Turn dataframe to SpatialPointsDataFrame, match projection
  adrpunti <- dat_grid
  coordinates(adrpunti) <- c("long_bin", "lat_bin")
  proj4string(adrpunti) <- proj4string(polygon_df) 
  
  # Obtain points inside the polygon 
  pt_in_poly <- sp::over(adrpunti, as(polygon_df, "SpatialPolygons"))
  
  # Make dataframe where TRUE means the point was outside of the polygon (i.e., buffer)
  outside_poly_df <- data.frame(outside_poly = is.na(pt_in_poly))
  
  # Join to vms dataframe, 
  dat_grid <- cbind(dat_grid, outside_poly_df)
  
  return(dat_grid)
  
}



