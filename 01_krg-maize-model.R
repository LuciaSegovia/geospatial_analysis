
# Cleaning the enviroment
rm(list=ls())

# Loading libraries ----
library(dplyr)
library(sp) # Spatial manipulation
library(sf) # Spatial manipulation
library(gstat) # Spatial modelling

# Loading the data -----
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Soil_Crop_comparisons", "Malawi",  "Malawi_grain_soil.xlsx"))

# checking data & variables
head(grain) 
summary(grain)
class(grain)
names(grain)
unique(grain$Crop)

# Checking values <LOD
grain$Se_triplequad[grain$Se_triplequad < 0.00269 & !is.na(grain$Se_triplequad)]

maize  <- subset(grain, Crop == "Maize")

# Generating spatial dataset ----
geodata.df  <- st_as_sf(maize, coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

# Checking missing values 
sum(is.na(geodata.df$Se_triplequad))
sum(is.na(geodata.df$pH_Wa))
sum(is.na(geodata.df$BIO1))


# Loading maize (all data)
# data.df  <- readRDS(here::here("data", "inter-output", "mwi-maize-se_LOD.RDS")) # only maize
data.df  <- readRDS(here::here("data", "inter-output", "mwi-grain-se_LOD.RDS"))

# Checking the data
names(data.df)
hist(data.df$Se_grain[!is.na(data.df$Se_grain)])
boxplot(Se_grain ~ Crop, data.df) # Crop values 
# Comparing maize from both surveys
boxplot(Se_grain[(!is.na(data.df$Crop) & data.df$Crop == "Maize" )|is.na(data.df$Crop)] ~ survey[(!is.na(data.df$Crop) & data.df$Crop == "Maize" )|is.na(data.df$Crop)], data.df)

# Adding crop to Chilimba
data.df$Crop[data.df$survey == "Chilimba"]  <- "Maize"

boxplot(Se_grain ~ Crop, data.df) # Crop values 

# Removing other crops for modelling 
# Converting crop into maize and Se into NA. 
maize.df  <- data.df


length(maize.df$Se_grain[maize.df$Crop != "Maize"]) # 204 (values to be modelled from other crops)
maize.df$Se_grain[maize.df$Crop != "Maize"]  <-  NA
maize.df$Crop[maize.df$Crop != "Maize"]  <-  "Maize_pred"
length(maize.df$Se_grain[maize.df$Crop == "Maize_pred"]) # Checking results
unique(maize.df$Crop)
sum(is.na(maize.df$Se_grain)) # 615 values would be predicted ()
dim(maize.df)

# Generating spatial dataset ----
geodata.df  <- st_as_sf(maize.df, coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

# Dataset for prediction
sedata.df  <- na.omit(geodata.df[, c("Se_grain", "pH_w", "BIO1")]) # No missing values for variables used in modeling
geodata.df <- subset(geodata.df, !is.na(pH_w) & !is.na(BIO1)) # to add predicted values

hist(sedata.df$Se_grain)

# Checking missing values 
sum(is.na(sedata.df$Se_grain))
sum(is.na(geodata.df$Se_grain))

# Variogram and fit variogram -----
vgm <- variogram(log(Se_grain) ~ pH_w + BIO1, sedata.df)
fit.vgm <- fit.variogram(vgm, vgm("Sph"))

krg <- krige(log(Se_grain) ~ pH_w + BIO1, sedata.df , geodata.df, model = fit.vgm)


# Add estimates to geodata.df -----
geodata.df$Se.krg <- krg$var1.pred
geodata.df$Se.krg.sd <- sqrt(krg$var1.var)

# back-log the predicuted values
geodata.df$pred.Se  <- exp(geodata.df$Se.krg)

plot(geodata.df[, "pred.Se"])
plot(geodata.df[, "Se_grain"])

sum(!is.na(geodata.df$pred.Se)) # n=1899 
sum(!is.na(geodata.df$Se_grain)) # n=1284 (1199 + 85)
sum(!is.na(geodata.df$Se_std)) # n=1806 
sum(!is.na(geodata.df$Se_std) & geodata.df$Crop == "Maize") # only maize 1603
sum(!is.na(geodata.df$Se_grain) & geodata.df$survey == "Chilimba") # + 85 (n=1688)
sum(!is.na(geodata.df$pred.Se) & geodata.df$survey == "Chilimba") # 87

head(geodata.df)
names(geodata.df)

plot(geodata.df$Se_grain, geodata.df$pred.Se)


# Converting back from spatial obj to dataframe
data.df  <- geodata.df  %>% st_drop_geometry()  %>%  #removing geometry
            right_join(., maize.df)  # adding back the long/lat variable

# Finalising the data: Adding values to standardised Se from Chilimba (observed (Se_grain+LOD))
data.df$Se_std[data.df$survey == "Chilimba"]  <- pred.maize$Se_grain[pred.maize$survey == "Chilimba"] 
data.df$Se_std[data.df$Crop == "Maize_pred"]  <-  NA # Removing other crop values


# Saving the dataset -----
#saveRDS(grain.df, here::here("data", "inter-output", "mwi-predicted-maizeSe.RDS"))
saveRDS(data.df here::here("data", "inter-output",
 "mwi-predicted-maizeSe_LOD.RDS"))

# Loading predicted maize values 

pred.maize  <- readRDS(here::here("data", "inter-output", 
 "mwi-predicted-maizeSe_LOD.RDS"))

# Finalising the data: Adding values to standardised Se from Chilimba (observed (Se_grain+LOD))
# pred.maize$Se_std[pred.maize$survey == "Chilimba"]  <- pred.maize$Se_grain[pred.maize$survey == "Chilimba"] 
# pred.maize$Se_std[pred.maize$Crop == "Maize_pred"]  <-  NA # Removing other crop values

 pred.maize  %>%  #dplyr::filter(is.na(Se_grain))  %>% 
 dplyr::select(pred.Se, Se_grain, Se_std)  %>% View()

 pred.maize  %>% dplyr::filter(is.na(Se_grain))  %>% 
 dplyr::select(pred.Se, Se_grain, Se_std)  %>% 
 filter(pred.Se<Se_std)

head(pred.maize)

sum(is.na(pred.maize$Se_triplequad))

mean(pred.maize$pred.Se, na.rm = TRUE)
mean(pred.maize$Se_grain, na.rm = TRUE)
mean(pred.maize$Se_std, na.rm = TRUE)

# Distribution of samples
hist(log(pred.maize$Se_grain))
hist(log(pred.maize$Se_grain[pred.maize$survey=="Chilimba"]))

mean(pred.maize$pred.Se[!is.na(pred.maize$Se_triplequad)], na.rm = TRUE)
mean(geodata.df$pred.Se[is.na(geodata.df$Se_triplequad)])

mean(geodata.df$Se_triplequad, na.rm = TRUE)
min(geodata.df$Se_triplequad, na.rm = TRUE)

length(geodata.df$Se_triplequad[geodata.df$Se_triplequad < 0.0086 & !is.na(geodata.df$Se_triplequad)])
geodata.df$Se_triplequad[geodata.df$Se_triplequad < 0.0086 & !is.na(geodata.df$Se_triplequad)]
