# Cleaning the environment
rm(list = ls())

# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling

## Loading the data

file <- grep("plasma", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-mwi-lakes.RDS"))

# covariates selection

covar <- c("Se_mean", "wealth_quintile", "urbanity", 
           "BMI",  "AGE_IN_YEARS", "crp", "agp", "dist_to_lake")

# Formula for the model
form <- log(Plasma_Se) ~  log(Se_mean) +
  wealth_quintile + BMI + urbanity + 
  AGE_IN_YEARS +
  log(crp) + log(agp) + log(dist_to_lake) +
  f(survey_cluster1, model = 'iid')


models <- list()


for(i in 1:length(file)){
  
  plasma_se <- readRDS(here::here("data", "inter-output", "model",
                                  file[i])) %>%
    # Joining the variable distance to inland water body
    left_join(., dist) 
  
  # Renaming variable and checking indv. data
  plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
  sum(duplicated(plasma_se$unique_id))
  names(plasma_se)   
  
  plasma_se <-plasma_se %>%
    dplyr::select(Plasma_Se, covar, unique_id, region, 
                  survey_cluster1,  Latitude,  Longitude)
  
  
  plasma_se <- na.omit(plasma_se)
  
  print(dim(plasma_se))
  
  
  # random effect (w/o spatial corr.)
  m <- inla(form,
                     data = as.data.frame(plasma_se),
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                            return.marginals.predictor=TRUE),
                     control.predictor = list(compute = TRUE))
  
  
  # Storing results
  models[i] <- list(m)
  
  # Print i
  print(i)
}