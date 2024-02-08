################################################################################
#
#                     Sensitivit analysis: 
#        Buffer-distance from 1km to 60km in Malawi
#         
#        
#
#
################################################################################

# Cleaning the environment ----
rm(list=ls())

# Loading libraries and functions -----

#library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
library(stringr) # string data manipulation 


# Data: Shapefiles ----

# Admin Boundaries for Malawi 

# Plasma Se conc. ----

# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
plasma.df  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
  filter(!is.na(selenium))


names(plasma.df)

## Predicted Se conc.  ----

# Loading the data
# Predicted Se conc. (predicted in 01_maize-model.R)
predmaize.df  <- read.csv(here::here("data", "predicted","Se_raw_OK_maize.csv"))

names(predmaize.df)

predmaize.df$predSe <- exp(predmaize.df$Zhat)

# Getting only cluster location (to avoid duplicates), 
# renaming buffer as geometry for converting into spatial object
geopredmaize.df <-  st_as_sf( predmaize.df , coords =c("Longitude", "Latitude"),
                              crs = "EPSG:4326")

## Generatting the buffer areas ----
## Only one time needed!
# Getting the cluster and their centroid to generate the two buffered areas.
# GPS <- plasma.df %>% distinct(survey_cluster1, Longitude, Latitude)
# 
# # Transforming maize data.frame into a spatial object (geometry) 
# geogps <- st_as_sf(GPS , coords =c("Longitude", "Latitude"),
#                    crs = "EPSG:4326")
# 
# # Choice of buffers
# # buffer <- c(10, 25, 30) # we also tested 12km
# buffer <- seq(1,60, 5)
# 
# # Loop over the number of buffers:
# for(i in 1:length(buffer)){
#   
#   data.df <- GPS
#   # distance <- as.numeric(paste0(buffer[i], "000"))
#   distance <- buffer[i]*10^3
# #variable <- paste0("buffer", buffer[i]) 
# 
# # Buffer in meters (10km & 25km)
# data.df$buffer <- st_buffer(geogps$geometry, dist = distance)
# 
# # Saving the shapefile with the buffers
# st_write(data.df, here::here( "data", "sensitivity",
#                               "boundaries", 
#                               paste0("mwi_gps-buffer", buffer[i], ".shp")))
# 
# }

## Aggregation of maize --------

path <- here::here("data", "sensitivity",
                   "boundaries")

buff.dist <- sort(as.numeric(na.omit(unique(str_extract(list.files(path),
                                        "[:digit:]{1,2}")))))

geodata.df <- geopredmaize.df

Se <- grep("Se", names(geodata.df), value = TRUE)

for(i in 1:length(buff.dist)){
  
  buffer  <- st_read( paste0(path, "/mwi_gps-buffer", buff.dist[i], ".shp")) %>% 
    rename(survey_cluster1 = "srvy_c1")
  
  maize_buff <- st_join(geodata.df, buffer) 
  

  maize_buff %>% st_drop_geometry() %>% 
    group_by(survey_cluster1) %>% 
    summarise(Se_mean = mean(!!sym(Se)), 
              Se_sd = sd(!!sym(Se)), 
              Se_median = median(!!sym(Se)), 
              Se_iqr = IQR(!!sym(Se)), 
              Se_n = n()) %>% 
    arrange(Se_n) %>% 
    saveRDS(., here::here("data", "sensitivity",  
                          paste0("pred-maize-buffer", 
                                 buff.dist[i], ".RDS")))
  
}

# Adding district variable to plasma 

cluster.df <- readRDS(here::here("data", "inter-output", 
                                 "aggregation", "master-cluster-admin-level.RDS"))


plasma.df  <- plasma.df %>% left_join(., cluster.df %>% 
                                        select(survey_cluster1, ADM2_PCODE, ADM2_EN) %>% 
                                        distinct())

# Maize Se conc. (from 01_maize-aggregation.R)

file <- grep("maize", list.files(here::here("data", "sensitivity")), 
             value = TRUE)

# Generatting the file ----

# Loop to generate a file with plasma data and maize with each aggregation unit

for(i in 1:length(file)){
  
  maize.df <- readRDS(here::here("data", "sensitivity",
                                 file[i]))
  names(maize.df)
  
  data.df <- left_join(plasma.df, maize.df) 
  
  if(sum(is.na(data.df$selenium))>0){
    stop(paste0("Missing values in plamsa Se in ", file[i]))
    
  }
  
  if(sum(is.na(data.df$Se_mean))>0){
    stop(paste0("Missing values in maize Se in ", file[i]))
    
  }
  
  saveRDS(data.df, here::here("data", "sensitivity",   "model",  
                              paste0("plasma-", 
                                     file[i])))
  
}


## INLA -----


# Cleaning the environment
rm(list = ls())

# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling

## Loading the data

file <- gtools::mixedsort(grep("plasma", list.files(here::here("data", "sensitivity", "model")), 
             value = TRUE))

# covariates selection

covar <- c("Se_mean", "wealth_quintile", "urbanity", 
           "BMI",  "AGE_IN_YEARS", "crp", "agp")

# Formula for the model
form <- log(y) ~ -1 + Intercept +  log(Se_mean) +
  wealth_quintile + BMI + urbanity + 
  AGE_IN_YEARS +
  log(crp) + log(agp) + f(spatial.field, model = spde) +
  f(ID, model = 'iid')


models <- list()


for(i in 1:length(file)){
  
  plasma_se <- readRDS(here::here("data", "sensitivity", "model", file[i]))
  
  # Renaming variable and checking indv. data
  plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
  sum(duplicated(plasma_se$unique_id))
  names(plasma_se)   
  
  plasma_se <- plasma_se %>% dplyr::filter(urbanity == "2")
  
  plasma_se <-plasma_se %>%
    dplyr::select(Plasma_Se, covar, unique_id, region, 
                  survey_cluster1,  Latitude,  Longitude)
  
  
  
  plasma_se <- na.omit(plasma_se)
  
  print(dim(plasma_se))
  
  # Removing values = 0 or NA
  if(sum(plasma_se$Se_mean ==0)>0){
    stop(paste0("Zero values found in ", file[i]))
  }
  
  
  # Locations
  coord <- cbind(plasma_se$Longitude, plasma_se$Latitude)
  
  #Creating the mesh
  mesh <- inla.mesh.2d(loc = coord, 
                       max.edge = c(.5, 3), 
                       cutoff = c(0.001))
  
  # Projection matrix (A) obs.
  A <- inla.spde.make.A(mesh = mesh , loc = coord)
  
  ## Setting the SPDE model (Matern estimator) 
  # (alpha is related to the smoothness parameter)
  # No priors are set
  spde <- inla.spde2.matern(mesh = mesh,
                            alpha = 2 ,
                            constr = TRUE) # this is optional
  
  ## Setting the SPDE index (to store the random effect)
  spde.index <- inla.spde.make.index(name = "spatial.field",
                                     n.spde = spde$n.spde)
  
  # Covariate list
  covs <- plasma_se %>%  dplyr::select(covar) %>% as.list()
  
  N <- nrow(plasma_se)
  
  stack <- inla.stack(
    data = list(y = plasma_se$Plasma_Se), # specify the response variable
    
    A = list(1, 1, 1, A), # Vector of Multiplication factors for random and fixed effects              
    
    effects = list(
      
      Intercept = rep(1, N), # specify the manual intercept!
      
      X = covs, # attach the model matrix
      
      ID = plasma_se$survey_cluster1, # insert vectors of any random effects
      
      w = spde.index)) # attach the w 
  
  
  
  
  # inla calculations
  
  m <- inla(form, 
            data = inla.stack.data(stack),
            family = "gaussian",
            control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
            control.compute = list(cpo = TRUE, dic = TRUE))
  
  
  # Storing results
  models[i] <- list(m)
  
  # Print i
  print(i)
  
}


## Visualising model output ------

library(ggplot2)
library(ggregplot)
#install.packages("MCMCglmm")
library(stringr)
library(magrittr)

Efxplot(models)

INLADICFig(models)


