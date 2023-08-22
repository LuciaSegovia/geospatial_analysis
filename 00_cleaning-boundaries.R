# Cleaning the environment
rm(list = ls())

# Loading libraries

library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
library(raster) # (spatial) raster data manipulation
library(tmap) # (spatial) visualisation

#library(maptools) #spatial data manipulation
#library(rgeos)

##########################################################################################

# Loading Shapefilees 

# Admin Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# Loading EA boundaries data (shapefile) (as raster)
 ea  <-   rgdal::readOGR(here::here( "..", "PhD_geospatial-modelling",
  "data", "mwi-boundaries", "EN_NSO", "eas_bnd.shp"))


# Generating the district raster layer by 
#Aggregating the values by district and at country level for the model
dist_bnd <- raster::aggregate(ea,  "DISTRICT") # Aggregate boundaries at district level
malawi_bnd <- raster::aggregate(ea) # Aggregate boundariesthe whole country

# Checking the data
plot(malawi_bnd)
head(malawi_bnd)


class(ea_bnd)
head(ea_bnd)

## Checking the mean area of EAs (km^2) and min buffer (2km radius = 12.57km^2) (area circ. = pi * r^2)
mean(ea_bnd$AREA_KM) # 13.51 
summary(ea_bnd$AREA_KM) # 3-10 sq. km
hist(ea_bnd$AREA_KM) # 13.51 
unique(ea_bnd$DISTRICT[ea_bnd$AREA_KM > 314.1593]) 
ea_bnd$EACODE[ea_bnd$AREA_KM > 314.1593] # 16 EAs including two major lakes and national parks

n  <- 5
m  <- 2

tm_shape(ea_bnd) +
tm_polygons() +
tm_shape(ea_bnd$geometry[ea_bnd$AREA_KM > (pi*(n^2))]) +
tm_polygons(col = "red") +
tm_shape(ea_bnd$geometry[ea_bnd$AREA_KM < (pi*(m^2))]) +
tm_polygons(col = "green")


# Area by district (4159.4 sq km) ~ 36km radius 
dist_bnd  <- ea_bnd  %>% group_by(DISTRICT)  %>% 
summarise(area = sum(AREA_KM)) 

mean(dist_bnd$area)
sqrt(4159.4/pi)

plot(dist_bnd$geometry)

tm_shape(dist_bnd$geometry)  %>% 
tm_polygons()


# Generating the shapefile for use in the model (e.g, inla.R)
rgdal::writeOGR(dist_bnd,  # object to be saved
here::here("..", "PhD_geospatial-modelling",   #folder for storing (all) shapefiles 
"data", "mwi-boundaries",   "EN_NSO"),   
   "dist_bnd",  # Name of the shapefile 
    driver="ESRI Shapefile")  # type of file (shapefile)

