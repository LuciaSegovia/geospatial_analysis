# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
 
# Loading the datat
# Plasma Se & Maize Se
dhs_se  <- readRDS(here::here("data","inter-output","dhs_se.rds")) 
maize_se  <- readRDS(here::here("data", "inter-output","mwi-maize-se.RDS")) # cleaned geo-loc maize Se data

dim(dhs_se)
names(dhs_se)
class(maize_se)

sum(duplicated(dhs_se$unique_id))
length(unique(dhs_se$survey_cluster1))

#Removing values missing Plasma Se value (exc. 34)
dhs_se  <-  subset(dhs_se, !is.na(selenium))
length(unique(dhs_se$survey_cluster1))

#There are 28 that have no sdistrict. 
count(is.na(dhs_se$sdist))

# Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

names(ea_bnd)
class(ea_bnd)

# Mean area for each EA
mean(ea_bnd$AREA_KM)
sd(ea_bnd$AREA_KM)

# TA
 ta_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries",
    "mwi_adm_nso_hotosm_20230329_shp", "mwi_admbnda_adm3_nso_hotosm_20230329.shp"))

names(ta_bnd)
class(ta_bnd)

# National parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))

View(ta_bnd)
head(ea_bnd)

table(sf::st_is_valid(ta_bnd))
table(sf::st_is_valid(ea_bnd))

plot(ta_bnd[, "ADM1_EN"])

plot(ea_bnd[, "EACODE"])

nso_bound  <- st_make_valid(ta_bnd) # Check this

# Selecting only variables that are interesting. (EA code, area, geo)
admin  <- ea_bnd[, c(4, 17, 18)]

sum(duplicated(admin$EACODE))
length(unique(admin$EACODE))

# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

data.df <- st_as_sf(maize_se , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

#Se_admin = st_intersection(data.df, admin)
Se_admin  <-  st_join(data.df, admin)

#Se_check = st_intersection(Se.df, nso_bound)

plot(Se_admin[, "Se_mg"])
plot(data.df[, "Se_mg"])
plot(data.df[, "Se_mg"])

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

boxplot(Se_mg ~ EACODE, Se_admin)
boxplot(pH ~ EACODE, Se_admin)

summary  <- Se_admin  %>% group_by(EACODE)  %>% 
summarise( count = n(Se_mg), 
          median_Se = median(Se_mg), 
          mean_logSe = mean(log_Se), 
          sd_Se = sd(Se_mg))

plot(summary[, "sd_logSe"])

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

ea_bnd  %>%  #filter(ADM2_EN == "Mzimba")  %>% 
tm_shape() +
tm_polygons() +
tm_shape(data.df) +
tm_symbols(size =0.3)

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
