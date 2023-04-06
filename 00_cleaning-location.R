# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Loading the datat
# Plasma Se
dhs_se  <- readRDS(here::here("data","inter-output","dhs_se.rds")) 

dim(dhs_se)
names(dhs_se)

sum(duplicated(dhs_se$unique_id))
length(unique(dhs_se$survey_cluster1))

#Removing values missing Plasma Se value
dhs_se  <-  subset(dhs_se, !is.na(selenium))
length(unique(dhs_se$survey_cluster1))

#There are 28 that have no sdistrict. 
count(is.na(dhs_se$sdist))

# Boundaries for Malawi 

# EAs
boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "echo2_prioritization" , 
 "ECHO2_prioritization.shp"))

names(boundaries)

# TA
 nso_bound  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries",
    "mwi_adm_nso_hotosm_20230329_shp", "mwi_admbnda_adm3_nso_hotosm_20230329.shp"))

# national parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))

View(nso_bound)
head(boundaries)

table(sf::st_is_valid(nso_bound))
table(sf::st_is_valid(boundaries))

nso_bound  <- st_make_valid(nso_bound)

# Selecting only variables that are interesting. 
admin  <- boundaries[, c(1:7, 27)]

sum(duplicated(admin$EACODE))
length(unique(admin$EACODE))


# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

Se.df  <- dhs_se
Se_admin = st_intersection(Se.df, admin)
Se_check = st_intersection(Se.df, nso_bound)

names(Se_check)
names(Se_admin)
dim(Se_admin)
dim(Se_check)
nrow(Se_admin) == nrow(Se.df)
sum(duplicated(Se_admin$unique_id))
subset(Se_admin, grepl("Likoma", DISTRICT))
subset(dhs_se, grepl("likoma", haven::as_factor(sdist)))
length(unique(Se_admin$EACODE))

#Checking district that are different between reported in DHS
# And assigned by geographic location. 

check  <- setdiff(Se_admin$sdist, Se_admin$DIST_CODE)

test  <- subset(Se_admin,
 haven::zap_labels(sdist) == "107", 
# DIST_CODE == "105",
select = c(unique_id, sdist, DIST_CODE, DISTRICT, selenium)) 

test  <- subset(dhs_se,
 haven::zap_labels(sdist) == "105", 
 select = c(unique_id, sdist, selenium)) 

#tm_shape(nso_bound) 

nso_bound  %>% filter(ADM2_EN == "Mzimba")  %>% 
tm_shape() +
tm_polygons() +
tm_shape(test) +
tm_symbols()

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

haven::as_factor(dhs_se$sdist)  %>% sort()

plot(check)


### Creating (small area) boundaries for Malawi ------

# Getting EAs centroids

test_centroid  <- st_point_on_surface(boundaries[, c(1,27)])

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
