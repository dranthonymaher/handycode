# # Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
# # You now have it in your current working directory, have a look!
# 
# # Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
# system("unzip DATA/world_shape_file.zip")
# #  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

library(rgdal)
my_spdf <- readOGR( 
  dsn= "C:/Users/amaher2/OneDrive - KPMG/Documents" , 
  layer="TM_WORLD_BORDERS-0.3",
  verbose=FALSE
)
summary(my_spdf)
length(my_spdf)
head(my_spdf@data)

plot(my_spdf)

# Basic plot of this shape file:
par(mar=c(0,0,0,0))
plot(my_spdf, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )


# 'fortify' the data to get a dataframe format required by ggplot2
library(broom)
library(maptools)
spdf_fortified <- tidy(my_spdf, region = "NAME")

# Plot it
library(ggplot2)
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() 



plot(2)
plot(c(0,1,2),c(2,3,4))
plot(c(0,1,1,0),c(0,0,1,1))
polygon(c(0,1,1,0),c(0,0,1,1), col='red')

df=as.data.frame(cbind(c(0,1,1,0),c(0,0,1,1)))
polygon(df,col='blue')

my_spdf$NAME[1]
my_spdf$LON
my_spdf$LAT

plot(my_spdf$LON,my_spdf$LAT)
polygon(my_spdf$LON,my_spdf$LAT, col='red')
