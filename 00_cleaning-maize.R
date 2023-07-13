
# Loading libraries
library(dplyr)
library(sp)
library(sf) # for reading in and writting shapefiles
#source("CEPHaStat_3.R")
# Loading data 
maize  <- readxl::read_excel(here::here("data", "maize",
 "AllanChilimbaFieldData_forLucia_20230615.xlsx"), sheet = 1)

#Checking data
head(maize)
names(maize)

#Renaming variable & adding info on data source
names(maize)[c(23, 28)]  <- c( "Se_mg", "pH")
maize$survey  <- "Chilimba_2009"

# Unit conversion
#maize$lat  <-  gsub('E |o', '', maize$lat)
#maize$long  <-  gsub('S |o', '', maize$long)
#
#maize$lat  <-  as.numeric(measurements::conv_unit(maize$lat, from = 'deg_dec_min', to = 'dec_deg'))
#maize$long   <-  as.numeric(measurements::conv_unit(maize$lat, from = 'deg_dec_min', to = 'dec_deg'))
#
#hist(maize$lat[maize$lat < 140000])
#hist(maize$long)
#hist(maize$long[maize$long < 140000])
#
#maize  <- subset(maize, lat < 140000 & long < 140000)
#
#cord.dec1  =  SpatialPoints(cbind( maize$long, maize$lat), proj4string = CRS("+proj=longlat"))
#

#Checking the spatial function
cord.dec1  =  SpatialPoints(cbind( maize$Longitude_DD, maize$Latitude_DD), proj4string = CRS("+proj=longlat"))
cord.dec2  =  subset(maize, select = c(Longitude_DD, Latitude_DD))  %>% st_as_sf(., coords =c("Longitude_DD", "Latitude_DD"), crs = "EPSG:4326")

# Plotting points
par(mfrow = c(1, 2))
plot(cord.dec1, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.dec2, axes = TRUE, main = "Lat-Long Coordinates", col = "red", cex.axis = 0.95)

# Selecting & renaming variables of interest: Se, pH & coord
maize.df <- subset(maize, !is.na(Se_mg), 
select = c(Se_mg, pH, survey,Longitude_DD, Latitude_DD ))   %>% 
rename(Longitude = "Longitude_DD", Latitude = "Latitude_DD")

# Generating spatial dataset
#maize.df  <- maize.df %>% 
#st_as_sf(., coords =c("Longitude_DD", "Latitude_DD"), crs = "EPSG:4326")

# Loading the data
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Soil_Crop_comparisons", "Malawi",  "Malawi_grain_soil.xlsx"))
names(grain) # checking variables

# Subsetting variables of interest: coord., Se (only in Maize) and pH.
grain <- subset(grain, Crop == "Maize",
select = c(Latitude, Longitude, Se_triplequad, pH_Wa))

#Checking data & removing NAs
hist(grain$Se_triplequad)
length(grain$Se_triplequad[grain$Se_triplequad > 200]) #408
grain$Se_triplequad[grain$Se_triplequad > 200]  <- NA # Recoding NA
grain.df  <- subset(grain, !is.na(Se_triplequad)) # Removing NAs

#Checking the dataset
head(grain.df)
names(grain.df)

# Renaming variable & adding info on data source
grain.df   <- grain.df  %>% rename(Se_mg = "Se_triplequad", pH = "pH_Wa")
grain.df$survey  <- "GeoNutrition_2018"
names(grain.df) # Checkin the renaming

# Saving sample sites
cord.dec1b  =  SpatialPoints(cbind(grain.df$Longitude, grain.df$Latitude), proj4string = CRS("+proj=longlat"))

# Checking sample site for both surveys
par(mfrow = c(1, 2))
plot(cord.dec1, axes = TRUE, main = "Chilimba Sample Sites", cex.axis = 0.95)
plot(cord.dec1b, axes = TRUE, main = "GeoNutrition Sample Sites", col = "red", cex.axis = 0.95)

# Generating spatial dataset
#grain.df  <- st_as_sf(grain.df , coords =c("Longitude", "Latitude"),
# crs = "EPSG:4326")

# Merging the two survey datasets
data.df  <- rbind(maize.df, grain.df)
names(data.df)

# Checking final dataset
str(data.df)
plot(data.df)
par(mfrow = c(1, 2))
hist(maize.df$Se_mg)
hist(grain.df$Se_mg)
hist(data.df$Se_mg)
hist(data.df$pH)
hist(log(data.df$Se_mg))

# Maize Se conc. - data manipulation ----
dim(data.df) # 1282
sum(is.na(data.df$Se_mg))
data.df$log_Se  <- log(data.df$Se_mg)

# Generating spatial dataset
#data.df  <- st_as_sf(grain.df , coords =c("Longitude", "Latitude"),
 #crs = "EPSG:4326")

# Data exploration:
#CEPHaStat.R is not working... (Check in RStudio)

saveRDS(data.df, here::here("data", "inter-output", "mwi-maize-se.RDS"))
