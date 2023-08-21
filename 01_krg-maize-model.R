
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

# Dataset for prediction
sedata.df  <- na.omit(geodata.df) # No missing values
geodata.df <- subset(geodata.df, !is.na(pH_Wa)&!is.na(BIO1)) # to add predicted values



# Variogram and fit variogram -----
vgm <- variogram(log(Se_triplequad) ~ pH_Wa + BIO1, sedata.df)
fit.vgm <- fit.variogram(vgm, vgm("Sph"))

krg <- krige(log(Se_triplequad) ~ pH_Wa + BIO1, sedata.df , geodata.df, model = fit.vgm)


# Add estimates to geodata.df -----
geodata.df$Se.krg <- krg$var1.pred
geodata.df$Se.krg.sd <- sqrt(krg$var1.var)

# back-log the predicuted values
geodata.df$pred.Se  <- exp(geodata.df$Se.krg)

plot(geodata.df[, "pred.Se"])
plot(geodata.df[, "Se_triplequad"])

sum(is.na(geodata.df$pred.Se))
sum(is.na(geodata.df$Se_triplequad))

head(geodata.df)
names(geodata.df)


# Converting back from spatial obj to dataframe
grain.df  <- geodata.df  %>% st_drop_geometry()  %>%  #removing geometry
            right_join(., grain)  # adding back the long/lat variable

            
# Saving the dataset -----
saveRDS(grain.df, here::here("data", "inter-output", "mwi-predicted-maizeSe.RDS"))

# Loading predicted maize values 

pred.maize  <- readRDS(here::here("data", "inter-output", "mwi-predicted-maizeSe.RDS"))

head(pred.maize)

sum(is.na(pred.maize$Se_triplequad))

mean(pred.maize$pred.Se, na.rm = TRUE)
mean(pred.maize$pred.Se[!is.na(pred.maize$Se_triplequad)], na.rm = TRUE)
mean(geodata.df$pred.Se[is.na(geodata.df$Se_triplequad)])

mean(geodata.df$Se_triplequad, na.rm = TRUE)
min(geodata.df$Se_triplequad, na.rm = TRUE)

length(geodata.df$Se_triplequad[geodata.df$Se_triplequad < 0.0086 & !is.na(geodata.df$Se_triplequad)])
geodata.df$Se_triplequad[geodata.df$Se_triplequad < 0.0086 & !is.na(geodata.df$Se_triplequad)]
