
# Libraries
# install.packages("rgdal")
# install.packages("maps")
# install.packages("mapdata")
# install.packages("geoR")

library(maps)
library(mapdata) 
library(sp)
library(sf)
library(nlme)
library(Matrix)
library(numDeriv)
source("CEPHaStat_3.R")
library(geoR)


# Selecting the boundaries (1-3; district to EA)
n  <- 3

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", n, ".shp")))
class(boundaries)

boundaries   <- rgdal::readOGR(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", n, ".shp")))
str(boundaries)

# maize data
data.df  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

data.df<-read.csv("data/Malawi_Grain.csv",header=T,stringsAsFactors = T)
data.df<-data.df[which(data.df$Crop=="Maize"),]
dim(data.df)
# converting to coordinates
UTM36S="+init=epsg:32736"

sp::CRS(SRS_string = "EPSG:4326")
sp::CRS(SRS_string = "EPSG:32736")

# REVIEW: The sp transformation is not working with our dataset
locs<-cbind(data.df$Longitude,data.df$Latitude)
loc_sp<-SpatialPoints(locs, proj4string=CRS("+proj=longlat +datum=WGS84"))
# loc_UTM <- spTransform(loc_sp, sp::CRS(SRS_string = "EPSG:32736"))
loc_UTM<- spTransform(loc_sp, sp::CRS(UTM36S))

data.df<-data.frame(cbind(loc_UTM@coords,data.df$Crop,data.df$Se_triplequad))
names(data.df)[names(data.df) == "coords.x1"] <- "Easting"
names(data.df)[names(data.df) == "coords.x2"] <- "Northing"
names(data.df)[names(data.df) == "V3"] <- "crop"
names(data.df)[names(data.df) == "V4"] <- "Sel_trp"
#names(data.df)[names(data.df) == "V5"] <- "Fe"
#names(data.df)[names(data.df) == "V6"] <- "Zn"
#names(data.df)[names(data.df) == "V7"] <- "Se_trp"
#write.csv(data.df,"maize.csv")

# data.df<-read.csv("maize.csv",header=T,stringsAsFactors = T)
data.df<-na.omit(data.df)
data.df$Sel_trp  <- as.numeric(data.df$Sel_trp)

summa(data.df$Sel_trp)
summaplot(data.df$Sel_trp)
max(data.df$Sel_trp)

data.df$Sel_trp[data.df$Sel_trp ==88888] <- NA
data.df$Sel_trp[data.df$Sel_trp ==77777] <- NA
data.df$Sel_trp[data.df$Sel_trp ==99999] <- NA

data.df<-na.omit(data.df)
names(data.df)

# preliminary data analysis

summa(data.df$Sel_trp)
summaplot(data.df$Sel_trp)

names(data.df)

# fit the model

model<-lme(Sel_trp~1, random=~1|EA, data=data.df)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# log transformation
data.df$selog<-log(data.df$Sel_trp)
se_admin$selog  <- log(se_admin$Se_triplequad)


# fit model to transformed data

model<-lme(selog~1, random=~1|EA, data=data.df)
model<-lme(selog~1, random=~1|ID_1, data=se_admin)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# output the results
summary(model)
fixef(model) # fixed effects
n  <- fixef(model) # fixed effects
re<-ranef(model) # random effects

names(re)[1]  <- "intercept"
re$mean_ea  <- exp(re$intercept-n)
re$mean_ea  <- re$intercept-n

re  <- tibble::rownames_to_column(re, var = "ID_1")
head(re)

re  %>% full_join(., admin)  %>% st_as_sf()  %>% 
ggplot() + 
  geom_sf(aes(fill = mean_ea))

write.csv(re, "re.csv") # save output 

