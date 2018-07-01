#https://rpubs.com/danielequs/199150

##' x = a shapefile (sp)

fortify_shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}
