# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Loading the datat
# Plasma Se
dhs_se  <- readRDS(here::here("data","inter-output","dhs_se.rds")) 

dim(dhs_se)
names(dhs_se)

sum(duplicated(dhs_se$unique_id))
length(unique(dhs_se$survey_cluster1))

#Removing values missing Plasma Se value
dhs_se  <-  subset(dhs_se, !is.na(selenium))
length(unique(dhs_se$survey_cluster1))

#There are 28 that have no sdistrict. 
count(is.na(dhs_se$sdist))

# Boundaries for Malawi 

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "echo2_prioritization" , 
 "ECHO2_prioritization.shp"))

names(boundaries)
# Selecting only variables that are interensting. 
admin  <- boundaries[, c(1:7, 27)]

sum(duplicated(test))
sum(duplicated(admin$EACODE))
length(unique(admin$EACODE))


# Getting info on the admin boudaries (EA/district level)
# Allocating Se values to each admin unit

Se.df  <- dhs_se
Se_admin = st_intersection(Se.df, admin)

names(Se_admin)
dim(Se_admin)
nrow(Se_admin) == nrow(Se.df)
sum(duplicated(Se_admin$unique_id))
subset(Se_admin, grepl("likoma", NAME_1))
length(unique(Se_admin$ID_3))

#Checking district

check  <- setdiff(Se_admin$sdist, Se_admin$DIST_CODE)

subset(Se_admin, sdist %in% check)

removed_id  <- setdiff(dhs_se$unique_id, Se_admin$unique_id)

removed_id  <- subset(dhs_se, unique_id %in% removed_id)
