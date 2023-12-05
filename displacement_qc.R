# Cleaning the environment
rm(list = ls())

library(dplyr) # data wrangling 
library(sf) #spatial data manipulation
library(tmap) # (spatial) visualisation

# Loading the function to assess misclassification
source(here::here("functions", "pts-missclass.R"))

# Loading the data
# Maize data 
maize.df <- readRDS(here::here("data", "inter-output",  "mwi_maize-se-raw_admin.RDS"))

# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
filter(!is.na(selenium))

head(data.df)
names(data.df)

# Loading Shapefiles 
# Info on Coord & projections (https://epsg.io/32736)
# Admin Boundaries for Malawi (EAs)
ea_bnd  <- st_read(here::here( "data",
                  "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))
# National parks
parks  <-  st_read(here::here( "data",
                               "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))


no_pop <- subset(ea_bnd, HOUSEHOLDS == 0 & TOTAL == 0)
lake_malawi <- subset(ea_bnd, EACODE == 0)
lakes <- subset(ea_bnd, grepl("lake", DISTRICT, ignore.case = TRUE))

# Removing lakes and areas recoded as with 0 population
ea_admin <- subset(ea_bnd, EACODE != 0 &
                     HOUSEHOLDS > 0 & TOTAL > 0)

ea_admin <- subset(ea_bnd, EACODE != 0)

#Checking No population locations 
tm_shape(ea_bnd$geometry[grep("^1", ea_bnd$EACODE)]) +
  tm_polygons() +
  tm_shape(no_pop$geometry[no_pop$EACODE == "10204902"]) +
  tm_polygons(col ="red") +
  tm_shape(parks) +
  tm_borders(col = "green")

tm_shape(ea_admin) +
  tm_polygons()

# EACODE to character
# ea_bnd$EACODE <- as.character(ea_bnd$EACODE)

# data.df  <- data.df %>%
#   dplyr::select( selenium, wealth_quintile, BMI, urbanity,
#               is_smoker, had_malaria, ANY_INFLAMMATION, 
#                 unique_id, survey_cluster1, URBAN_RURA, Longitude, Latitude) %>% 
#   filter(!is.na(selenium))

# No misclassification of urban & rural
data.df$unique_id[data.df$urbanity==2 & data.df$URBAN_RURA=="U"]

# Admin Boundaries for Malawi 
sum(duplicated(data.df$unique_id))
length(unique(data.df$unique_id))

# converting the DHS dataframe into spatial object
geodata <- st_as_sf(data.df, coords =c("Longitude", "Latitude"),
                    crs = "EPSG:4326")

# Preaparing the data for the function

#Selecting the polygons of the EAs in Malawi
# spd <- sf::as_Spatial(st_geometry(ea_bnd), IDs = as.character(ea_bnd$fid))
#plot(spd)

# Getting the EAs areas and their ids. into spatial object 
#mwi <- as(ea_bnd[, c("fid", "geometry")], 'Spatial')

mwi <- as(ea_admin[, c("fid", "geometry")], 'Spatial')
# Getting the dhs as spatial point dataframe
dhs <- as(geodata, 'Spatial')

## Need polygon value
df2 <- point_in_polygon_fun(dhs, mwi, ea_admin$EACODE, 
                           Rural_Code = 0, n_Approximation = 10000, 
                     NA_Option = 1)

df$V2 <- as.character(df$V2)

data.df[, c("unique_id", "selenium")] %>% cbind(., df) %>% 
  left_join(., ea_bnd, by=c("V2" = "EACODE"))

data.df[, c("unique_id", "selenium")] %>% cbind(., df2) %>% 
  left_join(., ea_bnd, by=c("V2" = "EACODE")) %>% 
  filter(grepl("lake", DISTRICT, ignore.case = TRUE)) %>% View()

Se_prob <- data.df[, c("unique_id", "selenium")] %>% cbind(., df2) %>% 
  left_join(., ea_bnd, by=c("V2" = "EACODE")) %>% 
 # dplyr::rename(EACODE = "V2") %>% 
  st_drop_geometry()



  
test <- data.df[, c("unique_id", "selenium")] %>% cbind(., df) %>% 
  left_join(., plasma.df)

table(test$V2 == test$EACODE)

tm_shape(spd) +
  tm_polygons() +
  tm_shape(rpt) +
  tm_dots(col="red")
