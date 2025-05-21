

## Loading shape files and custom spatial dataset

# Libraries  -------
library(dplyr)
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation

# Shapefiles
# EAs
ea_bnd  <- st_read(here::here("data",
                              "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this

# Lake
mwi_lake<- ea_bnd %>% filter(grepl("Lake Malawi", DISTRICT,
                                     ignore.case = TRUE)) %>% 
  dplyr::select(c(4, 10, 11, 17, 18)) %>% 
  mutate(across(DISTRICT, ~stringr::str_to_title(DISTRICT)))

# Cluster's EA info -----
# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
cluster.df <- readRDS(here::here("data", "inter-output", 
                                 "dhs_se_gps_admin.RDS"))  %>% 
  distinct(survey_cluster1, EACODE)

ea_admin <- st_read(here::here( "data", "inter-output", 
                                "boundaries", "mwi_admbnda_adm4_nso.shp"))  %>% 
  left_join(., cluster.df)


malawi_bnd_lakes <- st_union(ea_bnd) # Aggregate boundaries the whole country (with lakes)
malawi_bnd <- st_union(ea_admin) # Aggregate boundaries the whole country


# GPS Location  ----
# Loading the dataset
GPS <- st_read(here::here("data", "MWGE7AFL", "MWGE7AFL.shp")) # GPS location DHS
names(GPS)

# Renaming variables
GPS <- dplyr::rename(GPS, survey_cluster1='DHSCLUST', Latitude='LATNUM', 
                     Longitude='LONGNUM',  altitude_in_metres='ALT_GPS')
dim(GPS)

#  Generating the offset buffer

for(i in 1:nrow(GPS)){
  
  # Assigning buffer size (in m) acc. to Urban (U) or Rural (R)
  offset.dist<-ifelse(GPS$URBAN_RURA[i]=="U", 2000, 5000)
  
  # Generating the buffers around the centroids
  GPS$buffer[i] <- st_buffer(GPS$geometry[i], dist = offset.dist)
  
}

names(GPS)

# Transforming the list into spatial class
#GPS <- GPS %>% st_drop_geometry() 
#GPS$survey_cluster1 <- as.character(GPS$survey_cluster1)
#ea_admin$survey_cluster1 <- as.character(ea_admin$survey_cluster1)


GPS <- GPS %>% st_drop_geometry() %>% left_join(., ea_admin %>%  st_drop_geometry() %>% 
                           select(survey_cluster1, ADM2_EN)) 
GPS <- GPS %>% 
  filter(!is.na(ADM2_EN)) %>% distinct() %>% st_as_sf()

# GPS$buffer <- st_as_sfc(GPS$buffer)

# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this
