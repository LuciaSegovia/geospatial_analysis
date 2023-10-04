# Cleaning the environment
rm(list = ls())

library(dplyr) # data wrangling 
library(sf) #spatial data manipulation
library(tmap) # (spatial) visualisation
# Loading the function
source(here::here("functions", "pts-missclass.R"))

# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data
# for future test if works with (dhs_se_gps.rds)
# Explore the dataset
head(data.df)
names(data.df)

# Coord & projections
## https://epsg.io/32736

# Loading Shapefilees 

# Admin Boundaries for Malawi 
# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# EACODE to character
# ea_bnd$EACODE <- as.character(ea_bnd$EACODE)

data.df  <- data.df %>%
  dplyr::select( selenium, wealth_quintile, BMI, urbanity,
              is_smoker, had_malaria, ANY_INFLAMMATION, 
                unique_id, survey_cluster1, URBAN_RURA, Longitude, Latitude) %>% 
  filter(!is.na(selenium))

# No misclassification of urban & rural
data.df$unique_id[data.df$urbanity==2 & data.df$URBAN_RURA=="U"]

# Admin Boundaries for Malawi 
sum(duplicated(data.df$unique_id))
length(unique(data.df$unique_id))

geodata <- st_as_sf(data.df, coords =c("Longitude", "Latitude"),
                    crs = "EPSG:4326")

# Preaparing the data for the function

#Selecting the polygons of the EAs in Malawi
# spd <- sf::as_Spatial(st_geometry(ea_bnd), IDs = as.character(ea_bnd$fid))
#plot(spd)

mwi <- as(ea_bnd[, c("fid", "geometry")], 'Spatial')

dhs <- as(geodata, 'Spatial')

## Need polygon value
df <- point_in_polygon_fun(dhs, mwi, ea_bnd$EACODE, Rural_Code = 0, n_Approximation = 10000, 
                     NA_Option = 0)

df$V2 <- as.character(df$V2)

data.df[, c("unique_id", "selenium")] %>% cbind(., df) %>% 
  left_join(., maize.df, by=c("V2" = "admin_id")) %>% 
  filter(!is.na(intercept)) %>% count()
  
test <- data.df[, c("unique_id", "selenium")] %>% cbind(., df) %>% 
  left_join(., plasma.df)

table(test$V2 == test$EACODE)

tm_shape(spd) +
  tm_polygons() +
  tm_shape(rpt) +
  tm_dots(col="red")
