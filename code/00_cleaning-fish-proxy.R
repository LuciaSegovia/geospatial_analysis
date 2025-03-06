# Cleaning the environment
rm(list = ls())

# Loading libraries

library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
library(raster) # (spatial) raster data manipulation
library(tmap) # (spatial) visualisation

# Data 

# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
cluster  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
  filter(!is.na(selenium)) %>% 
  dplyr::select(survey_cluster1, Latitude, Longitude) %>% 
  distinct()

# Transforming maize data.frame into a spatial object (geometry) 
cluster <- st_as_sf(cluster , coords =c("Longitude", "Latitude"),
                    crs = "EPSG:4326")


## Dist to water bodies ------
#WorldPop raster
# Loading data distance to water bodies (WB) 
wb  <- raster(here::here("data", "covariates",  
                         "mwi_esaccilc_dst_water_100m_2000_2012.tif"))


#Checking projection WGS84
crs(wb)

#Visualising the MAT data & maize sample locations
tm_shape(wb) + 
  tm_raster(legend.show = FALSE) + 
  tm_shape(cluster) + 
  tm_symbols(size = 0.1)


# Extracting distance from WB from the raster for cluster sample loc
cluster$dist_to_wb  <- extract(wb, cluster)
head(cluster)

# Checking data
hist(cluster$dist_to_wb)
sum(cluster$dist_to_wb==0)

#Changing 0 to 0.0001 for log-transf
cluster$dist_to_wb[cluster$dist_to_wb==0] <- 0.0001

# Saving file with the distance per cluster
# cluster %>% st_drop_geometry() %>% 
#   saveRDS(., here::here("data", "inter-output", 
#                         "worldpop_cluster-distance-to-wb.RDS"))

## Distance to lake Malawi ------

# Loading data on Malawi lake boundaries
lakes <-  st_read(here::here("data",
                             "mwi-boundaries", "EN_NSO" , "eas_bnd.shp")) %>% 
  dplyr::filter(grepl("Lake", DISTRICT))

# Calculating distance to lake malawi
cluster$dist_to_lake <- as.vector(st_distance(lakes[5,], cluster))

# Checking data
hist(cluster$dist_to_lake)
sum(cluster$dist_to_lake==0)

# Saving the distance to lake Malawi
# cluster %>% st_drop_geometry() %>% 
#   saveRDS(., here::here("data", "inter-output", 
#                         "cluster-distance-to-mwi-lake.RDS"))

## Distance to lakes in Malawi ------
# Distance to main lakes
dist_to_lakes <- as.matrix.data.frame(st_distance(lakes, cluster))
#Getting the closest of the 5
cluster$dist_to_lake <- apply(dist_to_lakes, 2, min)

# Checking data
hist(cluster$dist_to_lake)
sum(cluster$dist_to_lake==0)

# Saving the distance to lake Malawi
# cluster %>% st_drop_geometry() %>% 
#   saveRDS(., here::here("data", "inter-output", 
#                         "cluster-distance-to-mwi-lakes.RDS"))


# Visualising the MAT data & maize sample locations
tm_shape(lakes) + 
  tm_polygons() +
  tm_shape(lakes[5,]) + 
  tm_polygons(col = "blue")
