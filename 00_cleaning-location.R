#######################################################################
#
#   Adding information of the level of aggregation for maize Se conc.
#     in Malawi, info needed to calculate the predicted-mean of Se conc.
#   for future use and for modelling. 
#
#
#####################################################################################################


# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
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

# TAs
 ta_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "mwi_adm_nso_hotosm_20230329_shp", "mwi_admbnda_adm3_nso_hotosm_20230329.shp"))

# National parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))
 
# Explore the shapefile
head(ea_bnd)

table(sf::st_is_valid(ta_bnd))
table(sf::st_is_valid(ea_bnd))
table(sf::st_is_valid(parks))

ta_bnd <- st_make_valid(ta_bnd) # Check this

sort(unique(ta_bnd$ADM2_PCODE))
length(unique(ta_bnd$ADM2_PCODE))


##################################################################################

#    Maize Se conc. 

############################################################################################################## 

# Loading the datat
#  Maize Se conc. (cleaned from 00_cleaning-maize.R)
data.df  <- readRDS(here::here("data", "inter-output","mwi-maize-se.RDS")) # cleaned geo-loc maize Se data

# Explore the dataset
head(data.df)
names(data.df)



# Getting the admin units (the one with the higher no of unique id/names)
dim(ea_bnd) #9235
sum(duplicated(ea_bnd$EACODE))
length(unique(ea_bnd$EACODE)) #9219
length(unique(ea_bnd$DISTRICT)) #28/30 #dist code/ district
length(unique(ea_bnd$TA_CODE)) #351/350 #ta code/ ta
sum(is.na(ea_bnd$TA_CODE)) # Checking NAs

# Checking comparabiity between the two boundaries dataset
# Even district are different
sort(unique(ea_bnd$DISTRICT))
sort(unique(ta_bnd$ADM2_EN))

#Commenting maps generation for speeding the processing
#tm_shape(ea_bnd) +
#tm_polygons("DISTRICT", show.legend = FALSE) 
#
#tm_shape(ta_bnd) +
#tm_polygons("ADM2_EN", show.legend = FALSE) 

# Selecting only variables that are interesting. (EA code, EA area, TA code, district & geo)
admin  <- ea_bnd[, c(4, 10, 11, 17, 18)]
head(admin)

# Generating the spatial object (geometry) from data
geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")


dim(geodata.df) #1282 - maize 804 plasma
# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

#Se_admin = st_intersection(data.df, admin)
Se_admin  <-  st_join(geodata.df, admin)

dim(Se_admin)
sum(is.na(Se_admin$EACODE))

missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]

#Checking missing values
#tm_shape(ea_bnd) +
#tm_polygons() +
#tm_shape(missing) +
#tm_symbols(col ="Se_mg", size =0.1)
#
st_join(missing[,1:5], admin, st_is_within_distance, 
            dist = units::set_units(4500, "m"))

# First one (row = 80 at 60m, EACODE == 31106023
# Second one (row = 346 at 200m, EACODE == 20701017) 
# Third one (row = 347 at 270m, EACODE == 20701017)
# Forth one (row = 82 at 4500m. EACODE == 31008901)
ea_bnd  %>% dplyr::filter(EACODE %in% c("31008901", "31001043")) %>% 
tm_shape() +
tm_polygons() +
tm_shape(missing[1,]) +
tm_symbols(col ="#df2020", size =0.1)

# Manually fixing missing EACODES
Se_admin[80,]
Se_admin[80,]  <- st_join(missing[,1:5], admin, st_is_within_distance, 
            dist = units::set_units(60, "m"))[1,]

missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]

Se_admin[346,]
Se_admin[346,]  <- st_join(missing[,1:5], admin, st_is_within_distance, 
            dist = units::set_units(200, "m"))[2,]

missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]

Se_admin[347,]
Se_admin[347,]  <- st_join(missing[,1:5], admin, st_is_within_distance, 
            dist = units::set_units(270, "m"))[2,]

missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]

Se_admin[82,]
Se_admin[82,]  <- st_join(missing[,1:5], admin, st_is_within_distance, 
            dist = units::set_units(4500, "m"))

(missing  <- Se_admin[which(is.na(Se_admin$EACODE)),])

# Checking the areas
sum(duplicated(Se_admin$EACODE))
length(unique(Se_admin$EACODE)) #9219 --> 1121 (not all EAs were sampled)
length(unique(Se_admin$DISTRICT)) #30 --> 26 (3 lakes + likoma island) # district
length(unique(Se_admin$TA_CODE)) #351 --> #ta code
sum(is.na(ea_bnd$TA_CODE)) # Checking NAs

# Converting back from spatial obj to dataframe
data.df  <- Se_admin  %>% st_drop_geometry()  %>% 
           right_join(., data.df) 

# Saving dataset with aggregation unit for modelling 
# Admin
aggregation  <- "admin"
file_name  <- paste0("mwi-maize-se_",aggregation, ".RDS")
saveRDS(data.df, here::here("data", "inter-output", file_name))


# Testing results accoring to different shapefile choices:

test  <- ea_bnd  %>% filter(!DISTRICT %in% unique(Se_admin$DISTRICT))
length(unique(test$DISTRICT))

#tm_shape(ea_bnd) +
#tm_polygons() +
#tm_shape(test) +
#tm_polygons("DISTRICT") +
#tm_shape(parks) +
#tm_borders(col = "green")

test  <- ea_bnd  %>% filter(!TA_CODE %in% unique(Se_admin$TA_CODE))
test  <- ta_bnd  %>% 
filter(!ADM3_PCODE %in% paste0("MW", unique(Se_admin$TA_CODE)))

test$TA_CODE  <- as.character(test$TA_CODE)
length(unique(test$TA_CODE))
length(unique(test$ADM3_PCODE))
length(unique(ta_bnd$ADM3_PCODE))

#tm_shape(ta_bnd) +
#tm_polygons() +
#tm_shape(test) +
#tm_polygons(col = "green", legend.show = FALSE) +
#tmap_options(max.categories = 124) +
#tm_shape(geodata.df) +
#tm_symbols(size = 0.09, col = "red")

##################################################################################

#    Plasma Se conc. 

############################################################################################################## 

# Loading the datat
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data
# Explore the dataset
head(data.df)
names(data.df)

# Admin Boundaries for Malawi 
sum(duplicated(data.df$unique_id))
length(unique(data.df$unique_id))

#Removing values missing Plasma Se value (exc. 34)
data.df <-  subset(data.df, !is.na(selenium))
length(unique(data.df$survey_cluster1))

geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

#There are 29 that have no sdistrict. 
sum(is.na(data.df$sdist))

plot(geodata.df[, "selenium"])

# Selecting only variables that are interesting. (EA code, EA area, TA code, district & geo)
admin  <- ea_bnd[, c(4, 10, 11, 17, 18)]
head(admin)

# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

#Se_admin = st_intersection(data.df, admin)
Se_admin  <-  st_join(geodata.df, admin)

dim(Se_admin)
dim(geodata.df)
sum(is.na(Se_admin$EACODE))
unique(Se_admin$EACODE)
length(unique(Se_admin$DISTRICT))
length(unique(Se_admin$sdist))

# Checking the areas
sum(duplicated(Se_admin$EACODE))
length(unique(Se_admin$EACODE)) #9219 --> 102 (not all EAs were sampled)
length(unique(Se_admin$DISTRICT)) #30 --> 26 (3 lakes + likoma island) # district
# length(unique(Se_admin$TA_CODE)) #351 --> 88 #ta code
# sum(is.na(ea_bnd$TA_CODE)) # Checking NAs

# Converting back from spatial obj to dataframe
data.df  <- Se_admin  %>% st_drop_geometry()  %>% 
           right_join(., data.df) 

# Saving dataset with aggregation unit for modelling 
# Admin
aggregation  <- "admin"
file_name  <- paste0("mwi-plasma-se_",aggregation, ".RDS")
saveRDS(data.df, here::here("data", "inter-output", file_name))



names(Se_check)
names(Se_admin)
dim(Se_admin)
dim(Se_check)
nrow(Se_admin) == nrow(data.df)
nrow(Se_admin) - nrow(data.df) # Loosing 4 rows.
sum(duplicated(Se_admin$unique_id))
subset(Se_admin, grepl("Likoma", DISTRICT))
subset(dhs_se, grepl("likoma", haven::as_factor(sdist)))
length(unique(Se_admin$EACODE))
length(unique(Se_admin$unique_id))


boxplot(selenium ~ EACODE, Se_admin)

class(Se_admin$EACODE)
Se_admin$EACODE  <- as.integer(Se_admin$EACODE)

ggplot(Se_admin, aes(EACODE, selenium)) + geom_point() +
facet_wrap(vars(DISTRICT))

Se_admin  %>% dplyr::count(DISTRICT, sdist, urbanity, EACODE)  %>% arrange(desc(DISTRICT))  %>% View()

boxplot(pH ~ EACODE, Se_admin)

summary  <- Se_admin  %>% group_by(EACODE)  %>% 
summarise( count = n(Se_mg), 
          median_Se = median(Se_mg), 
          mean_logSe = mean(log_Se), 
          sd_Se = sd(Se_mg))

plot(summary[, "sd_logSe"])

#Checking district that are different between reported in DHS
# And assigned by geographic location. 


test  <- subset(Se_admin,
 haven::zap_labels(sdist) == "105", 
# DIST_CODE == "105",
select = c(unique_id, sdist, DISTRICT, selenium)) 


check_location  <- subset(Se_admin, sdist %in% check, 
select = c(unique_id))  %>%  st_drop_geometry()  %>% pull()


subset(dhs_se, haven::zap_labels(sdist) == "105")

subset(Se_check, unique_id %in% check_location, 
            select =c(sdist, ADM2_EN))  %>% View()

subset(dhs_se, unique_id %in% check_location, 
            select =c(sdist, unique_id))

#107, 315, 313
haven::as_factor(dhs_se$sdist)[haven::zap_labels(dhs_se$sdist) == "210"]

removed_id  <- setdiff(dhs_se$unique_id, Se_admin$unique_id)

removed_id  <- subset(dhs_se, unique_id %in% removed_id)

haven::as_factor(dhs_se$sdist) %>% sort()

plot(check)


### Creating (small area) boundaries for Malawi ------

# Getting EAs centroids

test_centroid  <- st_point_on_surface(ea_bnd[, c(1,27)])

names(test_centroid)

#Checking that centroid have been created correclty
tm_shape(nso_bound) +
tm_polygons() +
tm_shape(test_centroid) +
tm_dots()

# Getting the buffer around the EAs (2km).

test_buffer2  <- st_buffer(test_centroid, dist =2000, endCapStyle = "SQUARE")
names(test_buffer2)

#Checking buffers around the centroid have been created correclty

tm_shape(nso_bound) +
tm_polygons() +
tm_shape(test_buffer2) +
tm_polygons(border.col = "red") 

tm_shape(nso_bound) +
tm_polygons() +
tm_shape(test_buffer2) +
tm_polygons(col = "red") +
tm_shape(park) +
tm_borders(col = "green") 

# Checking HH location (DHS) vs National Parks.


tm_shape(nso_bound) +
tm_polygons() +
tm_shape(dhs_se) +
tm_dots(col = "red") +
tm_shape(parks) +
tm_borders(col = "green") 

# Aggregation at HH level

# Getting the buffer around the HHS (10km).

test_buffer  <- st_buffer(dhs_se, dist =10000)
names(test_buffer)


#Checking buffers around the centroid have been created correclty

tm_shape(ea_bnd)+
tm_polygons() +
tm_shape(test_buffer) +
tm_polygons(border.col = "red") +
tm_shape(dhs_se) +
tm_dots() 



test_buffer  <-  st_join(dhs_se,  maize_se, st_is_within_distance,
            dist = units::set_units(10, "km"))

nrow(dhs_se)

nrow(test_buffer)


cellSize <- 0.001
grid <- (st_bbox(dhs_se) + cellSize/2*c(-1,-1,1,1)) %>%
  st_make_grid(cellsize=c(cellSize, cellSize)) %>% st_sf()

ggplot() + geom_sf(data=grid) + geom_sf(data=dhs_se)




## Testing OpenAI suggestion 

# Set up your point location
point_location <- data.frame(
  lon = -13.2543,
  lat = 34.3015
)

# Load the Malawi shapefile
malawi_shapefile <- st_read("path_to_malawi_shapefile/malawi.shp")

# Create a spatial point object
sp_point <- st_as_sf(point_location, coords = c("lon", "lat"), crs = 4326)

# Define the extent for the map
map_extent <- st_bbox(ea_bnd) + 0.1

# Create a square polygon around the point
buffer_distance <- 1 # Adjust the buffer distance as needed
buffer_poly <- st_buffer(dhs_se, dist = buffer_distance)

# Intersect the buffer polygon with the Malawi shapefile to constrain the boundaries
constrained_poly <- st_intersection(buffer_poly, ea_bnd)

# Divide the constrained polygon into quadrants
quadrants <- st_split(constrained_poly, st_as_sfc(st_bbox(constrained_poly), crs = st_crs(constrained_poly)), fill = TRUE)

# Plot the map (use quadrant instead of constrain poly)
ggplot() +
  geom_sf(data = ea_bnd, fill = "lightgray", color = "black") +
  geom_sf(data = constrained_poly, fill = "lightblue", color = "black") +
  geom_sf(data = sp_point, color = "red", size = 3) +
  coord_sf(xlim = c(map_extent$xmin, map_extent$xmax),
           ylim = c(map_extent$ymin, map_extent$ymax)) +
  theme_bw()


### Old Scripts

# Loading and selecting the boundaries (1-3; district to EA)
bn  <- 3

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", bn, ".shp")))


boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "echo2_prioritization" , 
 "ECHO2_prioritization.shp"))

names(boundaries)

# boundaries  <- boundaries  %>% filter(shapeID != "60268647B1308848342151") 
# Getting info on the admin boudaries (EA/district level)
# Using ID to avoid duplicates
name_var  <- paste0("ID_", bn)
admin  <- boundaries[, c("NAME_1",  name_var, "geometry")]
admin  <- boundaries[, c(1:7, 27)]
test  <- admin[, name_var]
#sum(duplicated(admin$ID_3))
sum(duplicated(test))
sum(duplicated(admin$EACODE))
length(unique(admin$EACODE))
plot(admin)

# Allocating Se values to each admin unit
# Choose the dataset:
#Se.df  <- maize_se 
Se.df  <- dhs_se[, c(1:4, 18, 22)] 
Se_admin = st_intersection(Se.df, admin)

names(Se_admin)
dim(Se_admin)
nrow(Se_admin) == nrow(Se.df)
sum(duplicated(Se_admin$unique_id))
subset(Se_admin, grepl("likoma", NAME_1))
length(unique(Se_admin$ID_3))

#Checking district

check  <- setdiff(Se_admin$sdist, Se_admin$DIST_CODE)

subset(Se_admin, sdist %in% check)

removed_id  <- setdiff(dhs_se$unique_id, Se_admin$unique_id)

removed_id  <- subset(dhs_se, unique_id %in% removed_id)

Se_admin %>% 
ggplot() + 
  geom_sf(aes(fill = selenium)) 

# Checking the points
 boundaries  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(removed_id) + 
  tm_symbols(col = "black") +
  tm_shape(Se_admin) + 
  tm_dots(col = "red")

  # Checking the points
 boundaries  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(dhs_se) + 
  tm_symbols(col = "black") +
  tm_shape(Se_admin) + 
  tm_dots(col = "red")



  # Visualisation of maize Se model results (Maps)

# Visual checks

ea_bnd$EACODE  <- as.character(ea_bnd$EACODE)
geoea.df  <- ea_bnd  %>% dplyr::select(EACODE, geometry)  %>% 
left_join(., ea.df)

#Checking the EAs w/ values
tm_shape(geoea.df) +
tm_polygons() +
tm_shape(parks)+
tm_polygons(col = "green") +
tm_shape(geoea.df) +
tm_polygons(col = "se_mean")