# library(maptools)
# library(sp)
library(sf)
# 
# mySA2s<-states=readShapePoly("C:/Users/amaher2/Downloads/meshblocks/SA2_2021_AUST_GDA2020.shp",verbose=TRUE)
# plot(mySA2s)


sa2_bnd <- st_read(
  "C:/Users/amaher2/Downloads/meshblocks/SA2_2021_AUST_GDA2020.shp")

install.packages("raster")
install.packages("rgdal")

library(raster)
library(rgdal)
aoiBoundary_HARV <- readOGR( "C:/Users/amaher2/Downloads/meshblocks/SA2_2021_AUST_GDA2020.shp")
class(aoiBoundary_HARV)
crs(aoiBoundary_HARV)
extent(aoiBoundary_HARV)
plot(aoiBoundary_HARV, col="cyan1", border="black", 
     main="AOI Boundary Plot")
