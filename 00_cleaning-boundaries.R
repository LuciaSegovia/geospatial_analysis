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

# Loading Shapefiles 

# Admin Boundaries for Malawi 

# Districts
dist_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries",
                              "mwi_adm_nso_hotosm_20230329_shp", 
                              "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))

# Admin Boundaries for Malawi 

# TAs
ta_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries",
                              "mwi_adm_nso_hotosm_20230329_shp", 
                              "mwi_admbnda_adm3_nso_hotosm_20230329.shp"))


# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# Removing lakes from boundaries dataset
# Selecting only variables that are interesting 
# fid, EA code, TA name, TA code, district, dist code, EA area & geometry)
ea_admin <- ea_bnd %>% filter(!grepl("lake", DISTRICT,
                                     ignore.case = TRUE)) %>% 
  dplyr::select(c(1, 4, 9, 10, 11, 12, 17, 18))

ea_admin$region <- NA

for(i in 1:3){
  ea_admin$region[grepl(paste0("^", i), ea_admin$EACODE)] <- i
}

ea_admin %>% filter(is.na(region))

# National parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries", "protected_areas_geo",
                              "protected_areas_geo.shp"))

# Explore the shapefile
head(ea_bnd)

table(sf::st_is_valid(ta_bnd))
table(sf::st_is_valid(ea_bnd))
table(sf::st_is_valid(parks))
table(sf::st_is_valid(dist_bnd))

ta_bnd <- st_make_valid(ta_bnd) # Check this
dist_bnd <- st_make_valid(dist_bnd) # Check this

sort(unique(ta_bnd$ADM2_PCODE))
length(unique(ta_bnd$ADM2_PCODE))

# Getting the admin units (the one with the higher no of unique id/names)
dim(ea_bnd) # 9235
sum(duplicated(ea_bnd$EACODE))
length(unique(ea_bnd$EACODE)) # 9219
length(unique(ea_bnd$DISTRICT)) #27/30 #dist code/ district
length(unique(dist_bnd$ADM2_EN)) #32 #district bc Blantyre, Lilongwe, Mzuzu and Zomba are divided into dist. and city
length(unique(ea_bnd$TA_CODE)) #351/350 #ta code/ ta
sum(is.na(ea_bnd$TA_CODE)) # Checking NAs

# Checking comparability between the two boundaries dataset
# Even district are different
sort(unique(ea_bnd$DISTRICT))
sort(unique(ta_bnd$ADM2_EN))

#Commenting maps generation for speeding the processing
# ea_bnd %>% 
# tm_shape() +
# tm_polygons() +
# ea_admin %>% filter(is.na(region)) %>% 
# tm_shape() +
# tm_polygons("DISTRICT", show.legend = FALSE) 

#ta_bnd %>% 
#dist_bnd %>% filter(grepl("Blantyre", ADM2_EN))  %>%  
#tm_shape() +
#tm_polygons("ADM2_EN", show.legend = FALSE) 


## Combining EAs with "authentic" District

test <-  st_join(ea_admin, dist_bnd)

sum(is.na(test$ADM2_PCODE))
sum(is.na(dist_bnd$ADM2_PCODE))
sum(is.na(test$EACODE))

test %>% filter(EACODE == "30299930")
ea_admin %>% filter(EACODE == "30111910")
ea_bnd %>% filter(DISTRICT == "Lake Chilwa")
ea_admin %>% filter(TA == "Kuluunda") %>% 
  tm_shape() +
  tm_polygons()

test$DISTRICT[test$DIST_CODE == "205"]

dist <- test$DISTRICT[is.na(test$ADM2_PCODE)]
eas <- test$EACODE[is.na(test$ADM2_PCODE)]

test$EACODE[is.na(test$ADM2_PCODE) & test$DISTRICT == "Mwanza"]

## Checking the "missing EAs" in their districts -----

# No.1 is in Mangochi, for some reason when removing the shape of the Namizimu Forest Reserve, Malaui
# The piece of land was removed. (to be added manually).
# No. 2 is a piece of land that seems to fall inside the lake it is "one EA" from the TA Kuluunda. It's also not
# reported in the DHS, so we are removing it. 
# No. 3 is in Salima too but I can't see it either...Also not in the DHS...
# No. 4 is in Mangochi & I can't see it, also not in the DHS
# No. 5 is in Mangochi & I can't see it, but in this case it is in the DHS (cluster = 23), so adding the district (check TA), 
# Funny enough, there is no TA assigned and acc. to docu there is no HH living in it... 
# No 6, 8 is the lake Chilwa, there is one EA on the top part of the lake,
# There's only one EA, and acc. to the data there is no HHs recorded.
# No. 8 is the EA in Neno, but it's located in Mwanza (I don't know why it doesn't pick it up)
# No. 9 seemed to be the same
# No.10 is in Chikwawa
## Solution--> Use the DISTRICT variable to complete ADM2_EN in all cases but in the Mwanza, as it should be Neno. 


i=7

tm_shape(test %>% filter(DISTRICT %in% dist[i])) +
  tm_polygons(border.col = "black") +
  tm_shape(dist_bnd %>% filter(ADM2_EN %in% c(dist[i]))) +
  tm_polygons("ADM2_EN", border.alpha = 0.01) +
#  tm_shape(test$geometry[is.na(test$ADM2_PCODE)]) +
  tm_shape(test$geometry[test$EACODE %in% eas[i]]) +
  tm_polygons( col = "red") 

ta_bnd %>% filter(ADM2_EN %in% c(dist[i])) %>% 
tm_shape() +
  tm_polygons("ADM3_EN") 

# Fixing the District Issues as per above -----
dist_fix <- gsub("Mwanza", "Neno", dist)

test$ADM2_EN[test$EACODE %in% eas]
test$ADM2_EN[is.na(test$ADM2_EN)] <- dist_fix

# Saving the new dataset with "correct" district for each EA ----

st_write(test, here::here( "data", "inter-output", 
           "boundaries", "mwi_admbnda_adm4_nso.shp"))


# eadist_bnd$boundaries_check <- ifelse(eadist_bnd$DISTRICT == eadist_bnd$ADM2_EN, TRUE, FALSE)

# eadist_bnd %>% filter(boundaries_check == FALSE) %>% count()

# Loading EA boundaries data (shapefile) (as raster)
 ea  <-   rgdal::readOGR(here::here( "..", "PhD_geospatial-modelling",
  "data", "mwi-boundaries", "EN_NSO", "eas_bnd.shp"))


# Generating the district raster layer by 
# Aggregating the values by district and at country level for the model
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
hist(ea_bnd$AREA_KM[ea_bnd$AREA_KM < 100]) # 13.51 
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



## Buffers

# centroids of all EAs
ea_cent  <- st_centroid(ea_bnd)

# Buffers

buffer_10  <- st_buffer(ea_cent, dist = units::set_units(10000, "m"))

tm_shape(ea_bnd$geometry[grep("^1", ea_bnd$EACODE)]) +
tm_polygons() +
tm_shape(buffer_10$geometry[grep("^1", buffer_10$EACODE)]) +
tm_borders(col = "red")


subset(ea_bnd, grepl("Likoma", DISTRICT))

sqrt(1/pi)*1000


pi*(0.8^2)

