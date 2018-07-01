################################################################################
##' @title Coastal buffer function
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2016-06-27
##' @log Add a log here
################################################################################

##' Functions takes as input:
##' vmsDat = dataset formatted to vmstools specifications 
##' removePings = logical vector that determines whether pings within 3nm of coast are removed. If FALSE, pings are identified, but not removed

library(rgdal)
library(sp)

### Use readOGR (rgdal package), permits subsetting
seas <- readOGR(dsn = "shapefiles/seas", layer = "World_Seas")
head(seas@data)

# Subset
adriatic <- seas[seas$NAME == "Adriatic Sea", ]

# vmsDat = datSeg %>% slice(1:100)

identify_adriatic_pings <- function(vmsDat) {
  
  ### Select points in Adriatic
  mapDat <- vmsDat
  
  # Change to spatial object
  coordinates(mapDat) <- c("SI_LONG", "SI_LATI")
  
  # Match the projection to the map of the adriatic sea
  proj4string(mapDat) <- proj4string(adriatic)
  
  # Obtain points inside the adriatic polygon (combine is.na() with over())
  adripoints <- !is.na(over(mapDat, as(adriatic, "SpatialPolygons")))
  adrp <- as.data.frame(adripoints)
  names(adrp) <- c("adriatico")
  adri <- cbind (vmsDat, adrp)
  vmsDat2 <- adri %>% filter(adriatico == TRUE)
  
  return(vmsDat2)
  
}

