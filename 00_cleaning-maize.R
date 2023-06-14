
library(dplyr)
library(sp)
library(sf) # for reading in and writting shapefiles

# Loading data 
maize  <- readxl::read_excel(here::here("data", "maize", "2009_mwi-maize.xls"))

#Checking data
head(maize)
names(maize)

#Renaming variable & adding info on data source
names(maize)[11]  <- "Se_mg"
maize$survey  <- "Chilimba_2009"

#Checking the spatial function
cord.dec1  =  SpatialPoints(cbind( maize$Longitude_DD, maize$Latitude_DD), proj4string = CRS("+proj=longlat"))
cord.dec2  =  subset(maize, select = c(Longitude_DD, Latitude_DD))  %>% st_as_sf(., coords =c("Longitude_DD", "Latitude_DD"), crs = "EPSG:4326")

# Plotting points
par(mfrow = c(1, 2))
plot(cord.dec1, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.dec2, axes = TRUE, main = "Lat-Long Coordinates", col = "red", cex.axis = 0.95)

# Generating spatial dataset
maize.df <- subset(maize, !is.na(Se_mg), select = c(9:11, 14))  %>% 
st_as_sf(., coords =c("Longitude_DD", "Latitude_DD"), crs = "EPSG:4326")

# Loading the data
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

# Subsetting variables of interest: only maize and Se.
grain <- subset(grain, Crop == "Maize",
select = c(Latitude, Longitude, Se_triplequad))

#Checking data & removing NAs
hist(grain$Se_triplequad)
length(grain$Se_triplequad[grain$Se_triplequad > 200]) #408
grain$Se_triplequad[grain$Se_triplequad > 200]  <- NA # Recoding NA
grain.df  <- subset(grain, !is.na(Se_triplequad)) # Removing NAs

#Checking the dataset
head(grain.df)
names(grain.df)

# Renaming variable & adding info on data source
names(grain.df)[3]  <- "Se_mg"
grain.df$survey  <- "GeoNutrition_2018"

# Saving sample sites
cord.dec1b  =  SpatialPoints(cbind(grain.df$Longitude, grain.df$Latitude), proj4string = CRS("+proj=longlat"))

# Checking sample site for both surveys
par(mfrow = c(1, 2))
plot(cord.dec1, axes = TRUE, main = "Chilimba Sample Sites", cex.axis = 0.95)
plot(cord.dec1b, axes = TRUE, main = "GeoNutrition Sample Sites", col = "red", cex.axis = 0.95)

# Generating spatial dataset
grain.df  <- st_as_sf(grain.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

# Merging the two survey datasets
data.df  <- rbind(maize.df, grain.df)

# Checking final dataset
str(data.df)
plot(data.df)
hist(maize.df$Se_mg)
hist(grain.df$Se_mg)
hist(data.df$Se_mg)

# Maize Se conc. - data manipulation ----

dim(data.df)
sum(is.na(data.df$Se_mg))
