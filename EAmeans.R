
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
bn  <- 3

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", bn, ".shp")))
class(boundaries)

boundaries   <- rgdal::readOGR(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", n, ".shp")))
str(boundaries)

# Loading biomarkers Se data
dhs_se  <- readRDS(here::here("data","inter-output",
"dhs_se.rds"))


# Allocating Se values to each admin unit

# Getting info on the admin boudaries (EA/district level)
#We changed from name to ID to avoid duplicates
name_var  <- paste0("ID_", bn)
admin  <- boundaries[, c("NAME_1", name_var, "geometry")]
sum(duplicated(admin$ID_3))
unique(admin$NAME_1)
plot(admin)

# Allocating plasma Se values to each admin unit
plasma_admin = st_intersection(dhs_se, admin)

names(plasma_admin)
dim(plasma_admin)
subset(plasma_admin, NAME_1 == "Balaka") 

# Calculating mean/median plasma Se by admin unit
plasma_mean   <- plasma_admin  %>% st_drop_geometry()  %>% 
dplyr::group_by(ID_3)  %>% 
dplyr::summarise(Se_median = median(selenium, na.rm = TRUE), 
          Se_mean = mean(selenium, na.rm = TRUE)) 

# Checkin the mean     
hist(plasma_mean$Se_mean)          
           
# Plotting the plasma Se median by admin unit
 plasma_mean   %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#plot()
ggplot() + 
  geom_sf(aes(fill = Se_median))

# Checking plasma values for the model
summaplot(plasma_admin$selenium)
sum(is.na(plasma_admin$selenium))
plasma_admin  <- subset(plasma_admin, !is.na(selenium)) # removing NA
plasma_admin$sel_log<-log(plasma_admin$selenium)
summaplot(plasma_admin$sel_log)

# fit the model: Plasma Se
model<-lme(sel_log~1, random=~1|ID_3, data=plasma_admin)


# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))


# log transformation
se_admin$selog  <- log(se_admin$Se_triplequad)


# fit model to transformed data

model<-lme(selog~1, random=~1|EA, data=data.df)

model<-lme(selog~1, random=~1|ID_1, data=se_admin)
#Fit model to ea
model<-lme(selog~1, random=~1|ID_3, data=se_admin)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# output the results
summary(model)
fixef(model) # fixed effects
n  <- fixef(model) # fixed effects
re <- ranef(model) # random effects

names(re)[1]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)


re  <- tibble::rownames_to_column(re, var = paste0("ID_", bn))
head(re)

hist(re$se_mean)

re  %>% full_join(., admin)  %>% st_as_sf()  %>% 
ggplot() + 
  geom_sf(aes(fill = se_mean)) #+
  # scale_fill_gradientn(colours=topo.colors(7),
    # limits=c(0.009, 0.17))


write.csv(re, "re.csv") # save output 

