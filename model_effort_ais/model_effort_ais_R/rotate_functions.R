## rotate the adriatic

#df = d_change

## Function to get 
get_utm_xy <- function(df, x = "SI_LONG", y = "SI_LATI"){
  
  library(rgdal)

  LatLong <- df %>% select_(x, y)
  names(LatLong) <- c("x","y")
  
  # Convert it to a sp object
  coordinates(LatLong) <- ~ x + y # longitude first
  
  # Add a coordinate reference system
  proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # Project using spTransform
  utm <- spTransform(LatLong, CRS("+proj=utm +zone=33 ellps=WGS84"))
  
  # Get dataframe
  df <- data.frame(x = coordinates(utm)[,1], y = coordinates(utm)[,2])
  
  return(df)

}

rotate_xy <- function(df, my_angle = 45, return_all = FALSE){
  
  df2 <- df %>% 
    mutate(xkm = x/1000, 
           ykm = y/1000, 
           xkm_center = mean(xkm), 
           ykm_center = mean(ykm), 
           angle = my_angle/180 * pi, 
           xr = xkm_center + cos(angle) * (xkm - xkm_center) + sin(angle) * (ykm - ykm_center), 
           yr = ykm_center - sin(angle) * (xkm - xkm_center) + cos(angle) * (ykm - ykm_center))
  
  if(return_all == FALSE){
    df2 <- df2 %>% select(xkm, ykm, xr, yr)
  }
  
  return(df2)
  
}

# 
# ## Example
# library(cowplot)
# p1 <- map_dat %>% 
#   ggplot(aes(SI_LONG, SI_LATI)) + geom_point(alpha = 0.1) + ggtitle("Not projected")
# p2 <- map_dat %>% 
#   ggplot(aes(xkm, ykm)) + geom_point(alpha = 0.1) + ggtitle("Projected (UTM zone 33)")
# p3 <- map_dat %>% 
#   ggplot(aes(xr, yr)) + geom_point(alpha = 0.1) + ggtitle("Projected and rotated 45 degrees")
# p_final <- plot_grid(p1, p2, p3, labels = c("A", "B", "C"), ncol = 3)
# p_final
# save_plot("model_effort_ais/model_effort_ais_figs/rotation_example.png", p_final, ncol = 3, nrow = 1, base_aspect_ratio = 1.3, base_height = 3.5)
