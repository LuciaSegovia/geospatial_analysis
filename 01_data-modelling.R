# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("CEPHaStat_3.R")) #stat functions
library(geoR)
#library(maps)
#library(mapdata) 
library(nlme)
library(Matrix)
library(numDeriv)



# Loading the datat
#check dhs.R script
dhs.df <- readRDS(here::here("data", "inter-output", "dhs.rds")) #cleaned dhs data
dhs_se  <- readRDS(here::here("data","inter-output","dhs_se.rds")) 

dim(dhs_se)
names(dhs_se)
sum(duplicated(dhs_se$unique_id))
#check maizeSe.R script
maize_se  <- readRDS(here::here("data", "inter-output","maize_se.rds")) # cleaned Spatial maize Se data

# Loading and selecting the boundaries (1-3; district to EA)
bn  <- 3

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", bn, ".shp")))


boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries",  "geoBoundaries-MWI-ADM3.shp"))

# boundaries  <- boundaries  %>% filter(shapeID != "60268647B1308848342151") 
# Getting info on the admin boudaries (EA/district level)
# Using ID to avoid duplicates
name_var  <- paste0("ID_", bn)
admin  <- boundaries[, c("NAME_1",  name_var, "geometry")]
admin  <- boundaries[, c("shapeName",  "shapeID", "geometry")]
test  <- admin[, name_var]
#sum(duplicated(admin$ID_3))
sum(duplicated(test))
unique(admin$NAME_1)
plot(admin)

# Allocating Se values to each admin unit
# Choose the dataset:
Se.df  <- maize_se 
#Se.df  <- dhs_se 
Se_admin = st_intersection(Se.df, admin)

names(Se_admin)
dim(Se_admin)
nrow(Se_admin) == nrow(Se.df)
sum(duplicated(Se_admin$unique_id))
subset(Se_admin, grepl("likoma", NAME_1))
length(unique(Se_admin$ID_3))

removed_id  <- setdiff(dhs_se$unique_id, Se_admin$unique_id)

removed_id  <- subset(dhs_se, unique_id %in% removed_id)

# Checking the points
 boundaries  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(removed_id) + 
  tm_symbols(col = "black") +
  tm_shape(Se_admin) + 
  tm_dots(col = "red")

  # Checking the points
 boundaries  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(dhs_se) + 
  tm_symbols(col = "black") +
  tm_shape(Se_admin) + 
  tm_dots(col = "red")

# Checking plasma values for the model
#Rename your variable:
names(Se_admin)
names(Se_admin)[1]  <- "selenium" 
# check for normality
summaplot(Se_admin$selenium)
sum(is.na(Se_admin$selenium))
Se_admin  <- subset(Se_admin, !is.na(selenium)) # removing NA
Se_admin$sel_log<-log(Se_admin$selenium)
summaplot(Se_admin$sel_log)

# fit the model: Plasma/Maize Se
model<-lme(sel_log~1, random=~1|shapeID, data=Se_admin)
model<-lme(sel_log~1, random=~1|NAME_1/ID_3, data=Se_admin)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# output the results
summary(model)
fixef(model) # fixed effects
n  <- fixef(model) # fixed effects
re <- ranef(model) # random effects

# output for nested outcome
re  <- re$NAME_1
re  <- re$ID_3
names(re)
names(re)[1]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)

re <- tibble::rownames_to_column(re, var = paste0("ID_", bn))
re <- tibble::rownames_to_column(re)
names(re)[1]  <- "NAME_1"
names(re)[1]  <- "NAME_1_ID_3"
names(re)[1]  <- "shapeID"
head(re)
hist(re$se_mean)

re$ID_3  <- stringr::str_replace(re$NAME_1_ID_3, "^[:alpha:]*[:punct:]", "")


re %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#plot()
ggplot() + 
  geom_sf(aes(fill = se_mean)) 

Se_admin  %>% st_drop_geometry()  %>% 
# group_by(across(all_of(name_var))) %>% 
 dplyr::group_by(NAME_1)  %>% 
dplyr::summarise(Se_median = median(selenium, na.rm = T))   %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#.[1,]  %>% 
ggplot() + 
  geom_sf(aes(fill = Se_median)) 
 
re_dist  <- re
re_ea  <- re

names(re_dist)[1]  <- "intercept"
re_dist$se_mean  <- exp(re_dist$intercept+n)
re_dist<- tibble::rownames_to_column(re_dist, var ="NAME_1")
head(re_dist)

write(re, "re_ea.csv")
write(re_dist, "re_ea_district.csv")

# Saving outputs of the model for plasma and maize
plasma_se  <- re
maize_se  <- re


#Joining them, so we get the adim areas with corresponding data
test  <- right_join(maize_se,  plasma_se, by = "ID_2")

#x - independent variable (explanatory) - maize Se
#y - dependent variable (response) - plasma Se
x  <-  test$se_mean.x  # maize Se
y  <-  test$se_mean.y  # plasma Se

plot(x, y)
abline(lm(y ~ x, data = test), col = "red")
