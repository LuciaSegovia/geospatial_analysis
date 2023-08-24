

library(dplyr)
library(sp)
library(sf) # for reading in and writting shapefiles
library(tmap) #visualising maps

# Excluded datasets -----

# Loading the data (Gashu - GeoNut)
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Soil_Crop_comparisons", "Malawi",  "Malawi_grain_soil.xlsx"))
names(grain) # checking variables

# Subsetting variables of interest: coord., Se (only in Maize), pH and  MAT (BIO1).
grain <- subset(grain, Crop == "Maize",
select = c(Latitude, Longitude, Se_triplequad, pH_Wa, BIO1))

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

# Adding MAT empty column to allow dataframes to merge
data.df$BIO1  <- NA

# Checking final dataset
str(data.df)
plot(data.df)
par(mfrow = c(1, 2))
hist(maize.df$Se_mg)
hist(grain.df$Se_mg)
hist(data.df$Se_mg)
hist(data.df$pH)
hist(log(data.df$Se_mg))

# Missing values check 
dim(data.df) # 1282
sum(is.na(data.df$Se_mg))
sum(is.na(data.df$pH)) # pH 2 missing values
sum(is.na(data.df$BIO1)) # 89 missing (to be completed)
#data.df$log_Se  <- log(data.df$Se_mg)


# Checking that values are the same as those provided in GeoNutrition 
BIOa  <- geodata.df$BIO1[!is.na(geodata.df$BIO1)]
BIOb  <- geodata.df$BIO1b[!is.na(geodata.df$BIO1)]
BIO_check  <- cbind(BIOa, BIOb)
head(BIO_check)

BIOa == BIOb
setdiff(round(BIOa), BIOb)
setdiff(BIOb, round(BIOa))
round(BIO_check[c(166, 167), ]) #Only one was 1degree dif. due to rounding

geodata.df  <- subset(geodata.df, select = -BIO1)  %>% # Removing BIO1 (orginal)
dplyr::rename(BIO1 = "BIO1b")  # Renaming extracted BIO1b to BIO1


# Loading data Joy et al 2012 -----
crop  <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =6, skip =2)

# Tidying table
# Getting variables names for the  1:8 variables
names(crop)[1:8]  <-  crop[7, c(1:8)]
names(crop)
# Removing empty/unnecessary rows
crop  <- crop[8:nrow(crop),]

# Select variables of interest
crop[, c("Sample_number", "X_coord", "Y_coord", "Crop", "Se")]

crop  <- crop  %>% select( c("Sample_number", "X_coord", "Y_coord", "Crop",
"FAO_soil_classification",  "Collection_notes",  "Se"))  %>% 
dplyr::filter(grepl("maize", Crop, ignore.case = TRUE) & is.na(Collection_notes))  %>%  # Selecting only maize grain, excluding flour
rename(FAO_soil ="FAO_soil_classification") # renaming

boxplot(Se ~ FAO_soil, crop)

# Loading data 
soil <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =5, skip =2)
names(soil)

# Select variables of interest
soil <- soil  %>% select(c("Sample_number", "pH", "FAO soil classifcation"))  %>% 
                                    filter(!is.na(pH))  %>% 
                                    rename(Soil_sample = "Sample_number", 
                                    FAO_soil = "FAO soil classifcation")

hist(soil$pH)
boxplot(pH ~ FAO_soil, soil)

# Loading data & Select variables of interest
crop_soil <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =10, skip =3)  %>%
 select(Sample_number, Soil_sample)  

# Merging soil and sample data

crop  <- crop  %>% left_join(., crop_soil) 

#Checking data
head(crop_soil)
head(soil)

tail(crop)

# Merging with pH info
crop  <- crop  %>% left_join(., soil, by = "Soil_sample")

# Issue with soil names

boxplot(pH ~ FAO_soil.x, crop) 
boxplot(pH ~ FAO_soil.y, crop) 


# Names of the FAO soils have different spelling
crop  %>% filter(!is.na(pH))  %>% select(starts_with("FAO_soil"))  %>% 
distinct()  %>%  View()

soil_name  <- crop  %>% filter(!is.na(pH))  %>% select(starts_with("FAO_soil"))  %>% 
distinct() 

#fixing soil names
for(i in 1:nrow(soil_name)){
crop$FAO_soil.x[crop$FAO_soil.x %in% soil_name[i,1]]  <- soil_name[i,2]
}

crop$FAO_soil  <- unlist(crop$FAO_soil.x)

boxplot(pH ~ FAO_soil, crop) 

crop1 <- crop  %>% filter(!is.na(X_coord) & !is.na(Y_coord))  %>% 
st_as_sf(., coords =c("X_coord", "Y_coord"), crs = "+init=epsg:32736")

#Changing the projection system so it is in line with other datasets (long/lat)
crop1 <- st_transform(crop,  crs = "+init=epsg:4326")

plot(crop1[, "FAO_soil"])
plot(crop1[, "Soil_sample"])

#tm_shape(crop1)+
#tm_symbols(col = "black", "Se" ) +
#tm_legend(show = FALSE) +
tm_shape(crop1)+
tm_symbols(col = "red", "pH") +
tm_legend(show = FALSE) 

