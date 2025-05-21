
# Cleaning the enviroment
rm(list=ls())

# Loading libraries
library(dplyr)
library(tidyr)
library(sp)
library(sf) # for reading in and writting shapefiles
library(raster) # raster manipulation
library(tmap)  #spatial data manipulation and visualisation
library(sfheaders) # 'deconstructing’ and ‘reconstructing’ sf objects
source("functions/CEPHaStat_3.R")


# Loading data (Kumssa - GeoNut) ----
# data.df  <- read.csv(here::here("data", "maize", "MWI_CropSoilChemData_CSV", # Grain & soil chem data
#  "MWI_CropSoilData_NA.csv"))
# Se_grain,Selenium,mg kg-1 DM
data.df  <- read.csv(here::here("data", "maize", "MWI_CropSoilChemData_CSV", # Grain & soil chem data
 "MWI_CropSoilData_Raw.csv"))
lod.df  <- read.csv(here::here("data", "maize", "MWI_CropSoilChemData_CSV", # Minerals LOD data
 "MWI_Crop_LOD_ByICPRun.csv"))

# Data checks
head(lod.df)
names(data.df)
names(lod.df)
names(lod.df)[25]  <- "Se_LOD"

# Getting variables of interest 1:12 (sample descript), grain_se and pH
data.df  <-  data.df[,c(1:12, 36, 95)]
# Adding survey info
data.df$survey  <- "GeoNutrion_Kumssa"

# Checking non-measured (NM) & replacing to NA ----
sum(grepl("NM",data.df$Se_grain))
data.df$Se_grain <- gsub("NM", NA, data.df$Se_grain)
# Transforming the variable into numeric
data.df$Se_grain <- as.numeric(data.df$Se_grain)

# Checking negative values & replacing to 0 ----
length(data.df$Se_grain[data.df$Se_grain<0 & !is.na(data.df$Se_grain)])
data.df$Se_grain[data.df$Se_grain<0 & !is.na(data.df$Se_grain)]  <-  0

names(data.df)
# Merging the LODs so we can add that info to the dataset
data.df  <- data.df  %>% left_join(., lod.df[, c(1, 25)], # merging grain & LOD datasets 
by = c("Crop_ICP_Run_Se" = "Crop_ICP_Run"))  %>%          # for Se
dplyr::rename(Se_raw = "Se_grain")  %>%                 # Renaming variable 
mutate(Se_grain = ifelse(Se_raw>Se_LOD, Se_raw, NA))  %>%  # Exc. all values below their LOD (converted into NA)
mutate(Se_std = ifelse(!is.na(Se_grain), Se_grain, Se_LOD), # Generating a variable that change NA to LOD values
       Se_zero = ifelse(is.na(Se_grain), 0, Se_grain))  # Getting the replaced to zero variable



#Checking differences between the two Se w/ NA variable
sum(is.na(data.df$Se_raw))
sum(is.na(data.df$Se_grain)) # 426 (NM+LOD) 
sum(is.na(data.df$Se_std)) 

#subset(data.df, !is.na(data.df$Se_grain) & is.na(data.df$LOD_Check)) # Double-checking that info above is correct

# Other checks
length(data.df$Se_std[is.na(data.df$Se_grain)])
mean(data.df$Se_std[is.na(data.df$Se_grain)])
min(data.df$Se_std[is.na(data.df$Se_grain)])
min(data.df$Se_raw, na.rm = TRUE)
min(data.df$Se_grain, na.rm = TRUE)

# Checking <LOD values
data.df$Se_grain[data.df$Se_grain < 0.00269 & !is.na(data.df$Se_grain)]
data.df$Crop[data.df$Se_grain < 0.00269 & !is.na(data.df$Se_grain)]

median(data.df$Se_grain, na.rm = TRUE)
sum(!is.na(data.df$Se_grain))
mean(data.df$Se_std, na.rm = TRUE)
hist(data.df$Se_std, na.rm = TRUE)

# No analysed values (NM in MWI_CropSoilData_Raw)
nm  <- c("MWI0574", "MWI1007","MWI1138","MWI1168","MWI1171","MWI1686")
subset(data.df, ID %in% nm)
# Changing LOD values for NA for non-analysed values
data.df$Se_std[data.df$ID %in% nm]  <-  NA
data.df$Se_zero[data.df$ID %in% nm]  <-  NA

# Data exploration

# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

 # Checking the NAs location
 plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "blue")
 plot(geodata.df$geometry[is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "red", add = TRUE)


# Subsetting only maize values (this step could be omitted if we want all crops)
#data.df  <- data.df  %>% filter(Crop == "Maize")

# Loading data (Chilimba) ----
maize  <- readxl::read_excel(here::here("data", "maize",
 "AllanChilimbaFieldData_forLucia_20230615.xlsx"), sheet = 1)

#Checking data
head(maize)
names(maize)
dim(maize)

#Renaming variable & adding info on data source
names(maize)[c(23, 28)]  <- c( "Se_mg", "pH_w")
maize$survey  <- "Chilimba"

# Getting variables of interest 1:12 (sample descript), grain_se and pH
maize  <-  maize[,c(1:11, 23, 28, 56)]

# Checking values 
sum(is.na(maize$Se_mg)) # Only 2
mean(maize$Se_mg, na.rm = TRUE)
min(maize$Se_mg, na.rm = TRUE)
max(maize$Se_mg, na.rm = TRUE)
summaplot(maize$Se_mg)
summaplot(log(maize$Se_mg))

#Checking the spatial function
cord.dec1  =  SpatialPoints(cbind( maize$Longitude_DD, maize$Latitude_DD), proj4string = CRS("+proj=longlat"))
cord.dec2  =  subset(maize, select = c(Longitude_DD, Latitude_DD))  %>% st_as_sf(., coords =c("Longitude_DD", "Latitude_DD"), crs = "EPSG:4326")

# Plotting points
par(mfrow = c(1, 2))
plot(cord.dec1, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.dec2, axes = TRUE, main = "Lat-Long Coordinates", col = "red", cex.axis = 0.95)

# Selecting & renaming variables of interest: Se, pH & coord
#maize.df <- subset(maize, !is.na(Se_mg), 
#select = c(Se_mg, pH, survey,Longitude_DD, Latitude_DD ))   %>% 
#dplyr::rename(Longitude = "Longitude_DD",  Latitude = "Latitude_DD")

maize.df <- maize  %>% 
dplyr::rename(Fields_loc = "Farmers fields", Cores = "Number of Cores", 
 Longitude = "Longitude_DD",  Latitude = "Latitude_DD", Se_grain = "Se_mg")  %>% 
 dplyr::select(-c(4, 8, 10))

names(maize.df)

# Generating spatial dataset for maize
geomaize.df  <- maize.df %>% 
st_as_sf(., coords =c("Longitude", "Latitude"), crs = "EPSG:4326")

plot(geomaize.df$geometry)

# Merging the two survey datasets -----

#data.df  <- rbind(data.df, maize.df)
data.df  <- bind_rows(data.df, maize.df) # using this bc not all variables are the same in both datasets
names(data.df)
head(data.df)

# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

 # Checking sample coverage location - Spatial pattern?
 plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "blue",  
      axes = TRUE)
 plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & is.na(geodata.df$Crop)], col = "green", add = TRUE)
 plot(geodata.df$geometry[is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "red", add = TRUE)
 plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & geodata.df$Crop != "Maize"], col = "yellow", add = TRUE )

 locsv<-c(-16.2,-16.5,-16.8, -17.1)
 locsv<-c(-9.5,-10,-10.5, -11)
 cols <- c("blue","green","red","yellow")
 points(35,locsv[1],pch=16, col=cols[1])
 points(35,locsv[2],pch=16, col=cols[2])
 points(35,locsv[3],pch=16, col=cols[3])
 points(35,locsv[4],pch=16, col=cols[4])
 
 text(35.2,locsv[1],"Maize grain (GeoNutrition)",pos=4)
 text(35.2,locsv[2],"Maize grain (Chilimba)",pos=4)
 text(35.2,locsv[3],"Maize grain (<LOD)",pos=4)
 text(35.2,locsv[4],"Other grains",pos=4)

# Adding covariate variable BIO1 (Mean Annual Temp - CHELSA dataset) ------

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
# REVIEW: values extracted as integer
geodata.df$BIO1  <- extract(mat, geodata.df)
head(geodata.df)

#Plotting the results
plot((geodata.df$BIO1)/10,geodata.df$Se_grain,pch=16,xlab="Mean annual temperature /°C",
ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")

# Saving the BIO back to dataframe (spatial obj to dataframe)

data.df  <- geodata.df %>% 
st_drop_geometry()  %>%  # dropping the geometry (spatial obj)
right_join(., data.df) 

#Checking again the data
dim(data.df) # 1282 # 1900
sum(!is.na(data.df$Crop) & data.df$Crop == "Rice")
sum(is.na(data.df$Se_grain) & data.df$survey == "Chilimba") # 2
sum(!is.na(data.df$Crop) & data.df$Crop != "Maize" & is.na(data.df$Se_grain)) # 409 (maize from GeoNut)
sum(is.na(data.df$Se_std) & data.df$survey == "Chilimba") #  6 from GeoNut & 88 from Chilimba
sum(is.na(data.df$pH_w)) # pH 1 missing values
sum(is.na(data.df$BIO1)) # completed

# Another way of getting back to dataframe
#maize.df <- sf_to_df(geomaize.df, fill = TRUE)   %>% 
#dplyr::rename(Longitude = "Longitude_DD",  Latitude = "Latitude_DD")  %>% 
#select = c(Se_mg, pH, BIO1, survey, Longitude, Latitude)

# Finalising the dataset
# Adding Chilimba Se data to variables: _raw and _std & Crop info
# REVIEW: Would it be adding more noise than helping the predictions?
sum(is.na(data.df$Se_raw) & data.df$survey == "Chilimba")
sum(is.na(data.df$Se_std) & data.df$survey == "Chilimba")
data.df$Se_grain[data.df$survey == "Chilimba"] 
data.df$Se_raw[data.df$survey == "Chilimba"] <- data.df$Se_grain[data.df$survey == "Chilimba"]
data.df$Se_std[data.df$survey == "Chilimba"] <- data.df$Se_grain[data.df$survey == "Chilimba"]
data.df$Se_zero[data.df$survey == "Chilimba"] <- data.df$Se_grain[data.df$survey == "Chilimba"]
data.df$Crop[data.df$survey == "Chilimba"] <-  "Maize"

# Adding year of the sampling

data.df$year <- ifelse(is.na(data.df$year), stringr::str_extract(data.df$SamplingEnd, "[:digit:]{4}$"), year)

sum(is.na(data.df$Se_zero))

# Saving dataset for modelling 
# saveRDS(data.df, here::here("data", "inter-output", "mwi-maize-se_LOD.RDS")) # only maize 
#saveRDS(data.df, here::here("data", "inter-output", "mwi-grain-se_LOD.RDS"))
saveRDS(data.df, here::here("data", "inter-output", "mwi-grain-se_raw.RDS"))
