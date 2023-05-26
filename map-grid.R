
# Load required packages
library(rgdal)
library(raster)
library(sf) #spatial data manipulation

#library(rgeos)
#library(dismo)


# Loading Boundaries for Malawi 

# Country
map  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "mwi_adm_nso_hotosm_20230329_shp" , 
 "mwi_admbnda_adm0_nso_hotosm_20230329.shp"))

map  <- readOGR(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "mwi_adm_nso_hotosm_20230329_shp" , 
 "mwi_admbnda_adm0_nso_hotosm_20230329.shp"), stringsAsFactors = F)

raster::crs(map)

# note the order of resolution (resolution in m)
grid <- raster(extent(map), resolution = c(0.1,0.1), 
crs = proj4string(map))

#crs = "+proj=longlat +datum=WGS84")
grid <- raster::extend(grid, c(100,5))

# class of grid
class(grid)

# convert to SpatialPolygonsDataFrame
gridPolygon <- rasterToPolygons(grid)
class(gridPolygon)

plot(map[, "ADM0_EN"])
plot(gridPolygon, add = T)
plot(gridPolygon)
