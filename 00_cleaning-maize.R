
# Loading libraries
library(dplyr)
library(sp)
library(sf) # for reading in and writting shapefiles
library(raster) # raster manipulation
library(tmap)  #spatial data manipulation and visualisation
library(sfheaders) # 'deconstructing’ and ‘reconstructing’ sf objects

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

mean(maize$Se_mg, na.rm = TRUE)
min(maize$Se_mg, na.rm = TRUE)
max(maize$Se_mg, na.rm = TRUE)

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
dplyr::rename(Longitude = "Longitude_DD",  Latitude = "Latitude_DD")

# Adding MAT empty column to allow dataframes to merge
maize.df$BIO1  <- NA

# Generating spatial dataset for maize
geomaize.df  <- maize.df %>% 
st_as_sf(., coords =c("Longitude", "Latitude"), crs = "EPSG:4326")

# Loading the data
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

names(maize.df)

# Merging the two survey datasets
data.df  <- rbind(maize.df, grain.df)
names(data.df)
head(data.df)

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

# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

# Data exploration:
#CEPHaStat.R is not working... (Check in RStudio)

# Adding the variable BIO1 for Chilimba loc. (Mean Annual Temp - CHELSA dataset)

# Loading data (MAT)
mat  <- raster(here::here("data", "covariates", "mwi_CHELSA_bio_10_1.tif" ))

#Checking projection WGS84
crs(mat)

#Visualising the MAT data & maize sample locations
tm_shape(mat) + 
tm_raster(legend.show = FALSE) + 
tm_shape(geodata.df) + 
tm_symbols(size = 0.1)


# Extracting MAT (BIO1) from the raster for maize sample loc
#REVIEW: values extracted as integer
geodata.df$BIO1b  <- extract(mat, geodata.df)
head(geodata.df)

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

#Plotting the results
plot((geodata.df$BIO1)/10,geodata.df$Se_mg,pch=16,xlab="Mean annual temperature /°C",
ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")

# Saving the BIO1 back to dataframe (spatial obj to dataframe)

data.df  <- geodata.df %>% 
st_drop_geometry()  %>%  # dropping the geometry (spatial obj)
right_join(., data.df  %>% dplyr::select(-BIO1)) # joinning back lon/lat excluding BIO1

#Chekcing again the data
dim(data.df) # 1282
sum(is.na(data.df$Se_mg))
sum(is.na(data.df$pH)) # pH 2 missing values
sum(is.na(data.df$BIO1)) # completed

# Another way of getting back to dataframe
#maize.df <- sf_to_df(geomaize.df, fill = TRUE)   %>% 
#dplyr::rename(Longitude = "Longitude_DD",  Latitude = "Latitude_DD")  %>% 
#select = c(Se_mg, pH, BIO1, survey, Longitude, Latitude)


# Saving dataset for modelling 

saveRDS(data.df, here::here("data", "inter-output", "mwi-maize-se.RDS"))



# Loading data (Kumssa - GeoNut) ----
data.df  <- read.csv(here::here("data", "maize", "MWI_CropSoilChemData_CSV",
 "MWI_CropSoilData_NA.csv"))
lod.df  <- read.csv(here::here("data", "maize", "MWI_CropSoilChemData_CSV",
 "MWI_Crop_LOD_ByICPRun.csv"))

head(lod.df)
names(data.df)
names(lod.df)
names(lod.df)[25]  <- "Se_LOD"

# Getting variables of interest 1:12 (sample descript), grain_se and pH
data.df  <-  data.df[,c(1:12, 36, 95)]

# Merging the LODs so we can add that info to the dataset

data.df  <- data.df  %>% left_join(., lod.df[, c(1, 25)], 
by = c("Crop_ICP_Run_Se" = "Crop_ICP_Run"))  %>% 
mutate(Se_std = ifelse(!is.na(Se_grain), Se_grain, Se_LOD), # Generating a varible that change NA to LOD values
LOD_Check = ifelse(Se_grain>Se_LOD, Se_grain, NA)) # Checking that all items below their LOD were converted into NA

#Checking differences between the two
sum(is.na(data.df$Se_grain))
sum(is.na(data.df$LOD_Check)) # this being higher means that some below detection were included (also found in M.L GeoNut dataset)

subset(data.df, !is.na(data.df$Se_grain) & is.na(data.df$LOD_Check)) # Double-checking that info above is correct

# Other checks
length(data.df$Se_std[is.na(data.df$Se_grain)])
mean(data.df$Se_std[is.na(data.df$Se_grain)])
min(data.df$Se_std[is.na(data.df$Se_grain)])
min(data.df$Se_grain, na.rm = TRUE)
min(data.df$Se_LOD, na.rm = TRUE)

# Checking <LOD values
data.df$Se_grain[data.df$Se_grain < 0.00269 & !is.na(data.df$Se_grain)]
data.df$Crop[data.df$Se_grain < 0.00269 & !is.na(data.df$Se_grain)]

mean(data.df$Se_std)

# No analysed values (NM in MWI_CropSoilData_Raw)
nm  <- c("MWI0574", "MWI1007","MWI1138","MWI1168","MWI1171","MWI1686")

subset(data.df, ID %in% nm)

data.df$Se_std[data.df$ID %in% nm]  <-  NA


# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

 # Checking the NAs location
 plot(geodata.df$geometry[!is.na(geodata.df$Se_grain)], col = "blue")
 plot(geodata.df$geometry[is.na(geodata.df$Se_grain)], col = "red", add = TRUE)
