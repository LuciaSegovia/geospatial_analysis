
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


# maize data
data.df<- readxl::read_excel(here::here("..", "GeoNutrition",
"Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

data.df<-data.df[which(data.df$Crop=="Maize"),]

# converting to coordinates
UTM36S="+init=epsg:32736"

# REVIEW: The sp transformation is not working with our dataset
locs<-cbind(data.df$Longitude,data.df$Latitude)
loc_sp<-SpatialPoints(locs, proj4string=CRS("+proj=longlat +datum=WGS84"))
loc_UTM<- spTransform(loc_sp, CRS(UTM36S))

data.df<-data.frame(cbind(loc_UTM@coords,data.df$Crop,data.df$Se_triplequad))
names(data.df)[names(data.df) == "coords.x1"] <- "Easting"
names(data.df)[names(data.df) == "coords.x2"] <- "Northing"
names(data.df)[names(data.df) == "V3"] <- "crop"
names(data.df)[names(data.df) == "V4"] <- "Sel_trp"
#names(data.df)[names(data.df) == "V5"] <- "Fe"
#names(data.df)[names(data.df) == "V6"] <- "Zn"
#names(data.df)[names(data.df) == "V7"] <- "Se_trp"
write.csv(data.df,"maize.csv")

data.df<-read.csv("maize.csv",header=T,stringsAsFactors = T)
data.df<-na.omit(data.df)


summa(data.df$Sel_trp)
summaplot(data.df$Sel_trp)
max(data.df$Sel_trp)

data.df$Sel_trp[data.df$Sel_trp ==88888] <- NA
data.df$Sel_trp[data.df$Sel_trp ==77777] <- NA
write.csv(data.df,"maize.csv")

data.df<-read.csv("maize.csv",header=T,stringsAsFactors = T)
data.df<-na.omit(data.df)
names(data.df)

# preliminary data analysis

summa(data.df$Sel_trp)
summaplot(data.df$Sel_trp)

# fit the model

model<-lme(Sel_trp~1, random=~1|EA, data=data.df)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# log transformation
data.df$selog<-log(data.df$Sel_trp)

# fit model to transformed data

model<-lme(selog~1, random=~1|EA, data=data.df)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# output the results
summary(model)
fixef(model) # fixed effects
re<-ranef # random effects
write.csv(re, "re.csv") # save output 

