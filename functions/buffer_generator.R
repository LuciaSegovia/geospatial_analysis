


## Generatting the buffer areas function ----

buffer_generator <- function(geo.df, min_b, max_b , increase, folder = NA){

# Data checks (if it's not spatial object)
 if(!sum(class(geogps) == "sf")>=1){
   print("The dataset is not an spatial object")
 }
  
# Loading library dependencies
library(sf)
  
# Generating the buffer of choice 
buffer <- seq(min_b,max_b, increase)
#buffer <- seq(10, 60, 10)

# Loop over the number of buffers:
 for(i in 1:length(buffer)){
   
   data.df<- geo.df
   distance <- buffer[i]*10^3
 
 # Genereting buffer in meters
 data.df$geometry <- st_buffer(geo.df$geometry, dist = distance)
 
 # Saving the shapefile with the buffers
 if(is.na(folder)){
   
   st_write(data.df, paste0("mwi_buffer", buffer[i], ".shp"))
 
}else{
   
   st_write(data.df, paste0(folder, "/mwi_buffer", buffer[i], ".shp"))

 }
 }
}

## Testing the function ----

# First, we need the centroid with its ids
## Need a spatial object!
# Getting the cluster and their centroid to generate the two buffered areas.
#GPS <- readRDS(here::here("data", "inter-output", 
#                          "dhs_se_gps_admin.RDS")) %>% distinct(survey_cluster1, Longitude, Latitude)
#
## Transforming maize data.frame into a spatial object (geometry) 
#geogps <- st_as_sf(GPS , coords =c("Longitude", "Latitude"),
#                   crs = "EPSG:4326")
#
## Districts
#dist_bnd  <- st_read(here::here( "data",
#                                 "mwi-boundaries",
#                                 "mwi_adm_nso_hotosm_20230329_shp", 
#                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
#dist_bnd <- st_make_valid(dist_bnd) # Check this
#
## Then, we need the buffer radious (min, max, and increase in km)
## max around the biggest district 60
## if buffer (circular) area = pi*r^2 
#sqrt((max(st_area(dist_bnd))/10^6)/pi) # around 60km
#
## Getting the folder path to save the output
##folder <- here::here("data", "inter-output", "boundaries", "buffer")
#folder <- here::here( "buffer")
#
## 10km min. displacement, 60km max. district area, 10km bins OK
# buffer_generator(geogps, 10, 60, 10, folder)
#   