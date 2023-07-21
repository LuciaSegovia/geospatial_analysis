
# Loading libraries

library(INLA) # Modelling (RINLA)
library(sf) #spatial data manipulation

# Loading the data
# Loading the plasma Se conc. dataset (cleaned from 01.cleaning-location.R)
data.df  <- readRDS(here::here("data", "inter-output","mwi-plasma-se_admin.RDS")) # cleaned geo-loc maize Se data

#Getting coord 
GPS <- st_read(here::here("data", "MWGE7AFL", "MWGE7AFL.shp"))  %>%  #GPS location DHS
dplyr::rename(survey_cluster1='DHSCLUST')

dim(GPS)

names(data.df)[10:20]

dim(data.df)
length(unique(data.df$survey_cluster1))

GPS  %>% st_drop_geometry()  %>%  count(ADM1SALBCO)

#INLA model preps

# Creating the mesh

geom  <- data.df$geometry

mesh  <- inla.mesh.2d(loc = geom, max.edge=c(0.5, 1))
