#######################################################################
#
#   Adding information of the level of aggregation for maize Se conc.
#     in Malawi, info needed to calculate the predicted-mean of Se conc.
#   for future use and for modelling. 
#
#
#####################################################################################################
# Cleaning the enviroment
rm(list=ls())

# Loading libraries and functions

#library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


###########################################################################################


#    Shapefiles


##########################################################################################

# Loading Shapefiles 

# Admin Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))


# National parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))
 
# Explore the shapefile
head(ea_bnd)

table(sf::st_is_valid(ea_bnd))
table(sf::st_is_valid(parks))


#############################################################################################################

#    Maize Se conc. 

################################################################################

## Loading the data ----

#  Maize Se conc. (cleaned from 00_cleaning-maize.R)
#file_name  <- "maize-se" # cleaned geo-loc maize Se data
# maize_values  <- "predicted-maizeSe_LOD" # cleaned  predicted maize Se data (from 01_krg-maize-model.R)
# file_name  <- paste0("mwi-", maize_values, ".RDS")
# data.df  <- readRDS(here::here("data", "inter-output", file_name)) 
data.df  <- readRDS(here::here("data", "inter-output", "mwi-grain-se_raw.RDS")) 


# Explore the dataset
head(data.df)
names(data.df)

length(data.df$Se_raw[!is.na(data.df$Se_raw) & data.df$Crop == "Maize"])

# Selecting only variables that are interesting. (EA code, EA area, TA code, district & geo)
admin  <- ea_bnd[, c(4, 10, 11, 17, 18)]
head(admin)

# Generating the spatial object (geometry) from data
geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")


dim(geodata.df) #1282 - maize 804 plasma # GeoNut 1900
# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

#Se_admin = st_intersection(data.df, admin)
Se_admin  <-  st_join(geodata.df, admin)

dim(Se_admin)
sum(is.na(Se_admin$EACODE))

# 60   61  976 1893 1895
missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]

#Checking missing values
tm_shape(ea_bnd) +
tm_polygons() +
tm_shape(missing) +
tm_symbols(col ="red", size =0.1)

# Checkin the closest EA for those missing EA GPS loc. 
st_join(missing[,1:ncol(geodata.df)-1], admin, st_is_within_distance, 
             dist = units::set_units(4500, "m"))  # %>% pull(EACODE)

m <-  c(90, 200, 300, 4500)
# Fixint missing (Pred. Se values) 
Se_admin[which(is.na(Se_admin$EACODE)),]

for(i in 1:length(m)){

Se_admin[which(is.na(Se_admin$EACODE)),]  <-  st_join(Se_admin[which(is.na(Se_admin$EACODE)),1:ncol(geodata.df)-1], 
    admin, st_is_within_distance,  dist = units::set_units(m[i], "m")) 

}

#missing  <- Se_admin[which(is.na(Se_admin$EACODE)),]



# Checking the areas
sum(duplicated(Se_admin$EACODE))
length(unique(Se_admin$EACODE)) #9219 --> 1628 (not all EAs were sampled)
length(unique(Se_admin$DISTRICT)) #30 --> 26 (3 lakes + likoma island) # district
length(unique(Se_admin$TA_CODE)) #351 --> #ta code
sum(is.na(ea_bnd$TA_CODE)) # Checking NAs

# Adding a variable for region (1>3 = N>S)
Se_admin$region  <-  NA
Se_admin$region[grepl("^1", Se_admin$EACODE)]  <- "1"
Se_admin$region[grepl("^2", Se_admin$EACODE)]  <- "2"
Se_admin$region[grepl("^3", Se_admin$EACODE)]  <- "3"

maize  <- subset(Se_admin, Crop=="Maize")

par(mfrow=c(1,3))
#boxplot(log(Se_grain) ~ region, Se_admin)
#boxplot(log(pred.Se) ~ region, Se_admin)
#boxplot(log(Se_std) ~ region, Se_admin)
boxplot(log(Se_grain) ~ region, maize)
boxplot(log(Se_std) ~ region, maize)
boxplot(log(Se_raw) ~ region, maize)

Se_admin$survey[Se_admin$Se_grain>1.5]
par(mfrow=c(1,2))
boxplot(Se_grain[Se_admin$Se_grain<1.5] ~ region[Se_admin$Se_grain<1.5], Se_admin)
boxplot(Se_std[Se_admin$Se_grain<1.5] ~ region[Se_admin$Se_grain<1.5], Se_admin)
boxplot(Se_raw ~ region, maize)
hist(maize$Se_raw)

# Converting back from spatial obj to dataframe
data.df  <- Se_admin  %>% st_drop_geometry()  %>%  #removing geometry
            right_join(., data.df)  # adding back the long/lat variable

# Saving dataset with aggregation unit for modelling 
# Admin 
#file_name  <- paste0("mwi-", maize_values, "_admin.RDS")
#saveRDS(data.df, here::here("data", "inter-output", file_name))
saveRDS(data.df, here::here("data", "inter-output", 
                            "mwi-grain-se_raw_admin.RDS"))


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

################################################################################

#    Plasma Se conc. 

################################################################################

# # Loading the data
# maize 
# file_name  <- paste0("mwi-", maize_values, "_admin.RDS")
# maize.df  <- readRDS(here::here("data", "inter-output", file_name))
maize.df  <- readRDS(here::here("data", "inter-output",
                                "mwi-grain-se_raw_admin.RDS"))

names(maize.df)
# Selecting only values measured in maize
unique(maize.df$Crop)
maize.df  <- subset(maize.df, Crop == "Maize")

maize.df %>% left_join(., ea_bnd) %>% dplyr::filter(is.na(EACODE))  

geomaize.df <- maize.df %>% left_join(., ea_bnd) %>%
  dplyr::select(EACODE, DISTRICT, region, geometry)

class(geomaize.df)

geomaize.df <- st_as_sf(geomaize.df)

#crs(geomaize.df$geometry)

#geomaize.df <- geometry(geomaize.df$geometry)

sum(is.na(geomaize.df$EACODE))

dim(maize.df)
#dim(geomaize.df)
#names(geomaize.df)
#head(geomaize.df)
#geomaize.df <- subset(geomaize.df, !is.na(pH_w))

# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data
# Explore the dataset
head(data.df)
names(data.df)

data.df  <- data.df   %>% dplyr::select( selenium, wealth_quintile, BMI, urbanity,
is_smoker, had_malaria, ANY_INFLAMMATION, unique_id, survey_cluster1, Longitude, Latitude)

# Admin Boundaries for Malawi 
sum(duplicated(data.df$unique_id))
length(unique(data.df$unique_id))

#There are 29 that have no sdistrict. 
#sum(is.na(data.df$sdist))

plot(data.df[, "selenium"])

#Removing values missing Plasma Se value (exc. 34)
geodata.df <-  subset(data.df, !is.na(selenium) ,
                     select = c(survey_cluster1, Longitude, Latitude)) %>% 
  distinct()

length(unique(geodata.df$survey_cluster1)) # After removing NA in plasma se (3 less clusters)
dim(geodata.df)

sum(duplicated(geodata.df$survey_cluster1))

geodata.df <- st_as_sf(geodata.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")


# Selecting only variables that are interesting. (EA code, EA area, TA code, district & geo)
# admin  <- ea_bnd[, c(4, 10, 11, 17, 18)]
# head(admin)

# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

#Se_admin = st_intersection(data.df, admin)
# Se_admin  <-  st_join(geodata.df, admin)
Se_admin  <-  st_join(geodata.df, geomaize.df)
dim(Se_admin)

sum(duplicated(Se_admin$survey_cluster1))

# Removing duplicates
Se_admin  <- distinct(Se_admin)

Se_admin[, c(1:5)]

dim(Se_admin)
dim(geodata.df)
sum(is.na(Se_admin$EACODE))
unique(Se_admin$EACODE)
length(unique(Se_admin$DISTRICT))
length(unique(Se_admin$sdist))

missing  <- Se_admin %>% filter(is.na(Se_admin$EACODE)) %>% 
  mutate(meter = NA)

#Checking missing values
tm_shape(ea_bnd) +
  tm_polygons() +
  tm_shape(missing) +
  tm_symbols(col ="red", size =0.1)

# Checking the closest EA for those missing EA GPS loc. 
 st_join(missing[,1:ncol(geodata.df)-1], geomaize.df, st_is_within_distance, 
        dist = units::set_units(4970, "m"))  %>% #filter(!is.na(EACODE)) %>% 
 filter(survey_cluster1 == "459")



m <- 30

for(i in 1:385){
  
  m <-  m+20
  print(m)
  
  }


for(i in 1:385){
  
  m <- m + 20
  
 test <-  st_join(missing[which(is.na(missing$EACODE)), 1:ncol(geodata.df)-1], 
              geomaize.df, st_is_within_distance,  
              dist = units::set_units(m, "m"))
  
  if(dim(test)[1]>0) {
  
    missing[which(is.na(missing$EACODE)),]  <-  st_join(missing[which(is.na(missing$EACODE)), 1:ncol(geodata.df)-1], 
                                                        geomaize.df, st_is_within_distance,  
                                                        dist = units::set_units(m, "m")) %>% 
    mutate(meter = m)
  }
  
  print(i)
  print(m)
  
}



sum(is.na(missing$EACODE))
sum(is.na(missing$meter))
sum(duplicated(missing$survey_cluster1))
no_missing <- distinct(missing)

id <- unique(missing$survey_cluster1)

missing  <- Se_admin %>% filter(is.na(Se_admin$EACODE) & 
                                  !survey_cluster1 %in% id) %>% 
  mutate(meter = NA)

# Repeat the loop

no_missing2 <- distinct(missing)

id <- unique(missing$survey_cluster1)

id <- c(id, unique(no_missing$survey_cluster1))

missing  <- Se_admin %>% filter(is.na(Se_admin$EACODE) & 
                                  !survey_cluster1 %in% id) %>% 
  mutate(meter = NA)

# Repeat the loop (if needed)

missing2 <- rbind(no_missing, no_missing2, distinct(missing)) 

# Checking duplicates

dupli <- missing2 %>% dplyr::count(survey_cluster1) %>% filter(n>1) %>% 
  pull(survey_cluster1)

# Removing (and manually adding (below)) duplicates

missing2 <- subset(missing2, !survey_cluster1 %in% dupli)

# Checking missing values 
Se_admin %>% st_drop_geometry() %>% 
  left_join(.,missing2 %>% st_drop_geometry(), by = "survey_cluster1") %>% 
  filter(is.na(EACODE.x) & is.na(EACODE.y)) # %>% 
 # pull(survey_cluster1)

# Getting the ids of the missing values (to be added manually, this includes the removed duplicates)
ids <- Se_admin %>% st_drop_geometry() %>% 
  left_join(.,missing2 %>% st_drop_geometry(), by = "survey_cluster1") %>% 
  filter(is.na(EACODE.x) & is.na(EACODE.y)) %>% 
pull(survey_cluster1)

# Checks
Se_admin %>% filter(is.na(EACODE)) %>% rbind(., missing2) %>% duplicated()
sum(is.na(Se_admin$EACODE))
sum(duplicated(Se_admin$survey_cluster1))
dim(Se_admin)

#cluster <- Se_admin$survey_cluster1
#cluster1 <- geodata.df$survey_cluster1

Se_admin[which(Se_admin$survey_cluster1 %in% id),]

missing2 %>% filter(survey_cluster1 == "571")
missing2 %>% filter(meter> 6000)


#  571 10302002 (5000m)
# 464 10501056 (4000m)
# 468 30805017 (2980m)
# 832 20908050 (1600m)
# 813 10102005 (3200m)
# 832 20908050 (1000m)

# 5800 [1]
Se_admin$meter <- NA # Adding variable to store distance (m)
# Checking
m <- 1000
st_join(Se_admin[which(Se_admin$survey_cluster1 %in% ids),1:ncol(geodata.df)-1],
geomaize.df, st_is_within_distance, 
dist = units::set_units(m, "m")) 

m <- 7600
st_join(Se_admin[which(Se_admin$survey_cluster1 %in% "497"),1:ncol(geodata.df)-1],
        geomaize.df, st_is_within_distance, 
        dist = units::set_units(m, "m")) 

Se_admin[which(is.na(Se_admin$EACODE)),]

m <- c(4000, 2980, 5000, 3200, 1000)
# Fixing missing (EACODE values) with a loop

for(i in seq_along(m)){
Se_admin[which(Se_admin$survey_cluster1 %in% ids[i]),] <- st_join(Se_admin[which(Se_admin$survey_cluster1 %in% ids[i]), 
                                                                  1:ncol(geodata.df)-1],
                                                    geomaize.df, st_is_within_distance, 
                                                    dist = units::set_units(m[i], "m"))[1,] %>%  mutate(meters = m[i])

}

# Checking the loop was effective
Se_admin[which(Se_admin$survey_cluster1 %in% ids),]

# Binding all values and distances (every cluster has a colocated EA w/ maize)
geocluster <- Se_admin %>% filter(!is.na(EACODE)) %>% rbind(., missing2) 

# merging with plasma Se dataset and removing missing Se
data <- data.df %>% left_join(., geocluster  %>% st_drop_geometry()) %>% 
  filter(!is.na(selenium)) #34 missing values (double check w/ original data)

# Saving plasma data with their EAs  (colocated with maize)
saveRDS(data, here::here("data", "inter-output", 
                            "mwi-plasma-se_maize-admin.RDS"))

## Loading data

data <- readRDS(here::here("data", "inter-output", 
                           "mwi-plasma-se_maize-admin.RDS"))

#Checking distances
hist(data$meter[data$urbanity == "2"])
sum(!is.na(data$meter) & data$urbanity == "2" & data$meter >5000) # 8 rural living WRA were located in EA >5km apart
table(data$urbanity)
count(data$survey_cluster1[!is.na(data$meter) & data$urbanity == "2" & data$meter >5000]) # 8 WRA in the same cluster were located in EA >5km apart
data$unique_id[data$survey_cluster1 == "497"]
data$meter[data$survey_cluster1 == "497"]

# checking cluster were either rural or urban
table(data$urbanity, data$survey_cluster1)
data$survey_cluster1[data$urbanity == 1 & data$urbanity == 2]
rural_id <- unique(data$survey_cluster1[data$urbanity == 2])

# Checking that all ids w/ Se values has a "co-located" EACODE. 
data %>% select(unique_id, EACODE) %>% left_join(., maize.df, 
                                                 by = "EACODE")

# Getting the EACODE for each plasma cluster & its centroid.

# Binding plasma EAs with EA boundaries file
plasma_bnd <- data %>% select(survey_cluster1, EACODE) %>% distinct() %>% 
  left_join(., ea_bnd) 

# Transforming the object into a spatial obj
plasma_bnd <- st_as_sf(plasma_bnd)

# Getting the centroids of each EA
plasma_centroid <-  st_centroid(plasma_bnd) 
plasma_centroidR <-  st_centroid(plasma_bnd) %>% filter(survey_cluster1 %in% rural_id)

# Drawing buffers around them (10km & 15km)
## should be smaller than district median (31km)
## 

plasma_buffer10 <- st_buffer(plasma_centroidR, dist = units::set_units(10000, "m"))
plasma_buffer15 <- st_buffer(plasma_centroidR, dist = units::set_units(15000, "m"))



#Checking centroids
tm_shape(ea_bnd) +
tm_polygons(col = "DISTRICT", legend.show = FALSE) +
tm_shape(plasma_buffer15) +
tm_polygons(col = "green", alpha = 0.5) +
tm_shape(plasma_buffer10) +
tm_polygons(col = "yellow", alpha = 0.5) +
  tm_shape(plasma_centroid) +
  tm_symbols(col ="red", size =0.1) +
  tm_shape(geodata.df$geometry[geodata.df$survey_cluster1 %in% rural_id]) +
  tm_symbols(col ="blue", size =0.1) 

# Check how many EAs per district (around 4 (1-8))
plasma_bnd %>% dplyr::count(DISTRICT)

# Check mean/median per district (around 4159.4(3093) (20-29185) 
ea_bnd %>% st_drop_geometry() %>%
   dplyr::group_by(DISTRICT) %>% 
  dplyr::summarise(area = sum(AREA_KM)) %>% 
  pull(area) %>% hist()

#Checking max buffer based on median district area.
area <-  3093
sqrt(area/pi)

m  <- 10

dataset[1,]  <- st_join(missing[1,], geomaize.df, st_is_within_distance, 
             dist = units::set_units(m, "m"))   %>% 
           #  dplyr::filter(!is.na(EACODE))  %>% 
             mutate(meters = m)


# Trying the loop

m <-  c(90, 200, 300, 4500)

# Fixint missing (Pred. Se values) 
Se_admin[which(is.na(Se_admin$EACODE)),]

for(i in 1:length(m)){

Se_admin[which(is.na(Se_admin$EACODE)),]  <-  st_join(Se_admin[which(is.na(Se_admin$EACODE)),1:ncol(geodata.df)-1], 
    admin, st_is_within_distance,  dist = units::set_units(m[i], "m"))

}


for(i in 1:nrow(missing)){

if(!is.na(dataset$EACODE[i])){

next

} 

m  <- m + 10

dataset[i,]  <- st_join(missing[i,], geomaize.df, st_is_within_distance, 
             dist = units::set_units(m, "m"))   %>% 
           #  dplyr::filter(!is.na(EACODE))  %>% 
             mutate(meters = m)

print(m)

}



#m <-  c(90, 200, 300, 4500)
# Fixint missing (Pred. Se values) 
Se_admin[which(is.na(Se_admin$EACODE)),]
Se_admin[which(is.na(Se_admin$EACODE)),]  <-  st_join(missing[,1:ncol(geodata.df)-1], admin, st_is_within_distance, 
             dist = units::set_units(m, "m"))


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


# Checking co-location at EA level for plasma and maize
plasma_se  <- readRDS(here::here("data", "inter-output", "mwi-plasma-se_admin.RDS"))
head(plasma_se)

test  <- plasma_se %>% filter(!is.na(selenium))  %>% select(-geometry)  %>%  
left_join(., Se_admin, by = "EACODE")

sum(is.na(test$selenium))
sum(is.na(test$pred.Se))

ea  <- unique(test$EACODE[is.na(test$pred.Se)]) #64

length(ea)/length(unique(test$EACODE))

tm_shape(ea_bnd) +
tm_polygons() +
tm_shape(test$geometry[!is.na(test$pred.Se)]) +
tm_symbols(col = "blue") +
tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% ea]) +
tm_polygons(col = "red")


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




# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data

data.df  <- data.df %>% dplyr::select( survey_cluster1, Longitude, Latitude)  %>% 
distinct()

head(data.df)




for(i in 1:nrow(data.df)){

 for (j in 1:nrow(data.df)) {
        # skip the computation if the two locations are the same
        if (i < j) {
            # calculate the distance between the two locations in km using the Vincenty ellipsoid method
            loc1 <- c(data.df$Longitude[i], data.df$Latitude[i])
            loc2 <- c(data.df$Longitude[j], data.df$Latitude[j])
            h[j] <- geosphere::distVincentySphere(loc1, loc2) / 1000 # distance converted to km
        }
 }
      data.df[i]  <- list(h)

 }