

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

dist <- readRDS(here::here("data", "inter-output", "worldpop_cluster-distance-to-wb.RDS"))
#dist$dist_to_wb[dist$dist_to_wb == 0] <-  200

# covariates selection

covar <- c("Se_mean", "wealth_quintile", "urbanity", 
           "BMI",  "AGE_IN_YEARS", "crp", "agp", "dist_to_wb")

# Formula for the model
form <- log(y) ~ -1 + Intercept +  log(Se_mean) +
  wealth_quintile + BMI + urbanity + 
  AGE_IN_YEARS +
  log(crp) + log(agp) + log(dist_to_wb) +
  f(spatial.field, model = spde) # +
  # f(ID, model = 'iid')


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

# plasma_se <- plasma_se %>% dplyr::filter(urbanity == "2")

plasma_se <-plasma_se %>%
  dplyr::select(Plasma_Se, covar, unique_id, region, 
         survey_cluster1,  Latitude,  Longitude)



plasma_se <- na.omit(plasma_se)

print(dim(plasma_se))

# Removing values = 0 or NA
if(sum(plasma_se$Se_mean ==0)>0){
  stop(paste0("Zero values found in ", file[i]))
}

#plasma_se <- na.omit(plasma_se)


# Locations
coord <- cbind(plasma_se$Longitude, plasma_se$Latitude)

#Creating the mesh
mesh <- inla.mesh.2d(loc = coord, 
                     max.edge = c(.5, 3), 
                     cutoff = c(0.001))

# Projection matrix (A) obs.
A <- inla.spde.make.A(mesh = mesh , loc = coord)
# Ap <- inla.spde.make.A(mesh = mesh , loc = coord) 

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


# Data structure
# IMPORTANT NOTE: order is important!!
# stack <- inla.stack(
#            data  = list(y = plasma_se$Plasma_Se),
#                     A = list(A, 1),
#                     effects = list(c(spde.index, 
#                             list (Intercept = 1)),
#                                    X=covs)
#                     , 
#                     tag = "obs")

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
