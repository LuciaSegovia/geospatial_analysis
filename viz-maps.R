

# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


###########################################################################################


#    Shapefiles


##########################################################################################

# Loading Shapefilees 

# Admin Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))


######################################################

# Loading Plasma Se with the EACODE allocated
data.df  <- readRDS(here::here("data", "inter-output","mwi-plasma-se_admin.RDS"))
maizedata.df  <- readRDS(here::here("data", "inter-output","mwi-maize-se.RDS"))

x1  <- subset(data.df, !is.na(selenium)) %>% distinct(EACODE)  %>% pull()
x2  <- as.integer(unique(maizedata.df$EACODE))

class(x2)

test_dif  <- setdiff(x1, x2)

sum(is.na(maizedata.df$Latitude))
sum(is.na(data.df$Se_mg))

geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

test  <- subset(geodata.df,
 survey_cluster1 == "309", 
 select = c(unique_id, DISTRICT, urbanity, EACODE, selenium, Se_mg)) 

test_centroid  <- st_point_on_surface(ea_bnd[, c(1,18)])
test_buffer  <- st_buffer(test_centroid, dist =1500) #1,5km

ea_bnd$fid[ea_bnd$EACODE == "10203024"] 



names(test_buffer)
#tm_shape(nso_bound) 

ea_bnd  %>%  filter(EACODE == "10203024")  %>% 
tm_shape() +
tm_polygons() +
tm_shape(test_centroid$geometry[test_centroid$fid=="197"])+
tm_symbols(size =0.2) +
tm_shape(test_buffer$geometry[test_buffer$fid=="197"]) +
tm_polygons(col = "red", alpha =.5) +
tm_shape(test) +
tm_symbols(shape = "urbanity", 
  size = "selenium") +
  tm_scale_bar()

dist  <- unique(ea_bnd$DISTRICT)



# Removing district 23 (Balaka), 25 (Likoma) bc no value w/o discrepancy & lakes
dist  <- dist[c(1:22, 24, 26:27) ]

dist[i]

maizedata.df   <-  st_as_sf(maizedata.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

data.df   <-  st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

# Loop that create maps per district w/ maize Se conc. & plasma Se conc.

for(i in 1:length(dist)){

test  <- Se_admin  %>% filter(DISTRICT == dist[i])
test2  <- data.df  %>% filter(DISTRICT == dist[i])

map  <- ea_bnd  %>% filter(DISTRICT == dist[i])  %>% 
tm_shape() +
tm_polygons() +
tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% ea & ea_bnd$DISTRICT == dist[i]]) +
tm_polygons(col = "red") +
tm_shape(test) +
tm_symbols(size = "pred.Se",
col = "blue") +
tm_shape(test2) +
tm_symbols(size = "selenium", 
col = "green") +
tm_layout( main.title = paste0(dist[i], " district"))

tmap_save(map, filename=paste0("visuals/map-pred_", dist[i], ".png"))

}

# # Loading the data
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Soil_Crop_comparisons", "Malawi",  "Malawi_grain_soil.xlsx"))
names(grain) # checking variables

grain.df   <-  st_as_sf(grain , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

grain.df   <-  grain.df  %>%
 filter(!is.na(Se_triplequad))
 
map  <- tm_shape(ea_bnd) +
tm_polygons() +
tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% test_dif]) +
tm_polygons(col = "red") +
tm_shape(grain.df) +
tm_symbols(col = "Crop", 
size =0.08) +
tm_layout(legend.show = FALSE) 

tmap_save(map, 
filename=paste0("visuals/map_GeoNut_se.png"))

data.df  <- data.df  %>% 
select(selenium, EACODE, survey_cluster1, unique_id, urbanity)  %>% 
left_join(., maizedata.df, 
by = c("EACODE"))

# Checking 
nrow(grain)
nrow(grain.df)
sum(!is.na(grain$pH_Wa))
sum(!is.na(grain$Zn))
sum(!is.na(grain$Se_triplequad))
