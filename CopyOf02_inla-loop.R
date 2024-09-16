

# Cleaning the environment
rm(list = ls())

# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling

## Loading the data

file <- grep("plasma.*v2.0.0", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-mwi-lakes.RDS"))
#dist$dist_to_wb[dist$dist_to_wb == 0] <-  200

# Covariates selection

covar <- c("Se_mean",
           "wealth_idx",
           #"wealth_quintile", 
          # "Malaria_test_result", "BMI",  
           "AGE_IN_YEARS", "crp", "agp", "urbanity", 
          "dist_to_lake")

# Formula for the model
form <- log(Plasma_Se) ~ -1 + a0  +  log(Se_mean) +
  # wealth_quintile +
  wealth_idx +
  age_scale +
  # BMI + Malaria_test_result +
  log(crp) + log(agp) + urbanity + log(dist_to_lake) +
 # f(spatial.field, model = spde)  +
   f(survey_cluster1, model = 'iid', hyper = hyper.idd, constr = TRUE)

# Model output
models <- list()
# Spatial output
# spde.est <- list()


for(i in 1:length(file)){

plasma_se <- readRDS(here::here("data", "inter-output", "model",
                                file[i])) %>%
  # Joining the variable distance to inland water body
                     left_join(., dist) 

  # Renaming variable and checking indv. data
plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
# sum(duplicated(plasma_se$unique_id))
# names(plasma_se)   

# class(plasma_se$survey_cluster1)
plasma_se$survey_cluster1 <- as.character(plasma_se$survey_cluster1)

# plasma_se <- plasma_se %>% dplyr::filter(urbanity == "2")

plasma_se <-plasma_se %>%
  dplyr::select(Plasma_Se, covar, unique_id, region, 
         survey_cluster1 #,  Latitude,  Longitude
         )

plasma_se$age_scale <- scale(plasma_se$AGE_IN_YEARS)

plasma_se <- na.omit(plasma_se)

print(dim(plasma_se))

# Removing values = 0 or NA
if(sum(plasma_se$Se_mean ==0)>0){
  stop(paste0("Zero values found in ", file[i]))
}

# Generating the intercept
plasma_se$a0 <- 1

# Hyperparameters
# The probability of having a SD> 0.1 is 50% (that's the meaning of 0.1, 0.5)
hyper.idd = list(theta1 = list(prior = "pc.prec", param = c(0.1, 0.5))) 
hyper.fix = list(theta1 = list(initial = log(100), fixed = TRUE))

# inla calculations

# random effect (w/o spatial corr.)
#malawi.iid <- inla(update(malawi.form, . ~ . + f(ID, model = "iid")),
#                   data = as.data.frame(test),
#                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
#                                          return.marginals.predictor=TRUE),
#                   control.predictor = list(compute = TRUE))

m <- inla(form, 
           family = "gaussian",
           data = as.data.frame(plasma_se),
          # this specify no. of CPU that can use 
          num.threads = 2,
           control.compute = list(cpo = TRUE, waic = TRUE, dic = TRUE,
          return.marginals.predictor=TRUE), 
      control.predictor = list(compute = TRUE))


# Storing results
models[i] <- list(m)


# Print i
print(i)

}
