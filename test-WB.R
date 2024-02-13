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

# Data 

# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
cluster  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
  filter(!is.na(selenium)) %>% 
  dplyr::select(survey_cluster1, Latitude, Longitude) %>% 
  distinct()

# Transforming maize data.frame into a spatial object (geometry) 
cluster <- st_as_sf(cluster , coords =c("Longitude", "Latitude"),
                        crs = "EPSG:4326")

names(plasma.df)

# Loading data (WB) water bodies
wb  <- raster(here::here("data", "covariates", "ESACCI", 
                          "ESACCI-LC-L4-WB-Map-150m-P13Y-2000-v4.0.tif" ))

#Checking projection WGS84
crs(wb)


## EAs in MWI
# bbox: xmin: 32.67162 ymin: -17.12628 xmax: 35.91842 ymax: -9.363662
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# bbox: xmin: 32.67162 ymin: -17.12628 xmax: 35.91842 ymax: -9.363662
st_bbox(ea_bnd)

#st_crs(ea_bnd) <- "+proj=longlat +datum=WGS84 +no_defs"

# Creating a box to cut (as per Malawi)
#xmn=32.67162,  xmx= 35.91842, ymn= -17.12628, ymx= -9.363662)
e <- as(extent(32, 36,  -18, -9), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
plot(e)

r <- crop(wb, e)
plot(r)

tm_shape(r) +
 tm_raster(legend.show = FALSE)  +
  tm_shape(ea_bnd) +
  tm_polygons()


mwi_r <-  terra::as.polygons(rast(r), dissolve = TRUE) %>% 
  st_as_sf()

names(mwi_r)[1] <- "water.land"

plot(mwi_r)
plot(mwi_r$geometry[mwi_r$water.land == "2"])

mwi_wb <- mwi_r %>% filter(water.land == "2")

# 
cluster$dist_to_wb <- as.vector(st_distance(mwi_wb, cluster))

tm_shape(r) +
  tm_raster(legend.show = FALSE)  +
  tm_shape(cluster) +
  tm_symbols(col = "dist_to_wb") +
  tm_layout(legend.outside = TRUE)

# mwi_wb <-  mask(wb, ea_bnd)
#plot(mwi_wb)

cluster %>% st_drop_geometry() %>% 
  saveRDS(., here::here("data", "inter-output", 
                        "cluster-distance-to-wb.RDS"))

# inland water bodies 
data  <- st_read(here::here( "data", "covariates", "GLWD2", "glwd_2.shp")) 
st_crs(data) <- "EPSG:4326"
sf_use_s2(FALSE)  # the input geometries are bad, and wrong spherically

data <- st_make_valid(data) # Check this
table(sf::st_is_valid(data))



# Malawi bnd
malawi  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm0_nso_hotosm_20230329.shp"))




lakes <- ea_bnd %>% dplyr::filter(grepl("Lake", DISTRICT))

st_crs(data) <- st_crs(malawi)

d1_mwi <- st_crop(data, ea_bnd)

tm_shape(d1_mwi) +
  tm_polygons() +
  tm_shape(lakes) +
  tm_polygons()
                 