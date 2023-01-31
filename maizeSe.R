
library(sp)
library(sf)

b_admin1  <- st_read(here::here("..", "PhD_geospatial-modelling","data", "mwi-boundaries", "gadm40_MWI_1.shp"))
b_admin1_sp <- sf::as_Spatial(b_admin1$geometry) 

plot(b_admin1_sp)

names(data.df)
names(b_admin1_sp)
coordinates(data.df) <- ~ Longitude + Latitude  

#proj4string(data.df) <- proj4string(b_admin1_sp) 

x  <- over(data.df, b_admin1_sp)

x  <- cbind(b_admin1_sp, over( b_admin1_sp, data.df))  

plot(x)


