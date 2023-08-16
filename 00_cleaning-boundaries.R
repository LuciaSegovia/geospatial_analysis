# Cleaning the environment
rm(list = ls())

# Loading libraries

library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
library(raster) # (spatial) raster data manipulation

#library(maptools) #spatial data manipulation
#library(rgeos)


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

# Generating the shapefile for use in the model (e.g, inla.R)
rgdal::writeOGR(dist_bnd,  # object to be saved
here::here("..", "PhD_geospatial-modelling",   #folder for storing (all) shapefiles 
"data", "mwi-boundaries",   "EN_NSO"),   
   "dist_bnd",  # Name of the shapefile 
    driver="ESRI Shapefile")  # typo of file (shapefile)


    
    
