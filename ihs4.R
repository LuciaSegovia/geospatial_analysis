
# Loading libraries
library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("CEPHaStat_3.R")) #stat functions


# Loading data
ihs4  <- read.csv()
gps  <- read.csv(here::here("data", "HouseholdGeovariablesIHS4.csv"))


## ) GPS data ----
head(gps)
names(gps)

# There are 3HHs with missing GPS coord. 
count(is.na(gps$lat_modified))
count(is.na(gps$lon_modified))
gps$HHID[is.na(gps$lat_modified)]
# Need to remove HHs w/o coord
gps  <- subset(gps, !is.na(lon_modified))
# Transform csv into spatial object

gps.sf  <- st_as_sf(gps, coords =c("lon_modified", "lat_modified"), crs = "EPSG:4326")

plot(gps.sf)
