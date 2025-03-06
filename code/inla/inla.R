# Cleaning the environment
rm(list = ls())

# Loading libraries

library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
#library(maptools) #spatial data manipulation
#library(rgeos)

# Loading Shapefiles

# Admin Boundaries for Malawi

# EAs
ea_bnd <- st_read(here::here(
  "..", "PhD_geospatial-modelling", "data", "mwi-boundaries",
   "EN_NSO", "eas_bnd.shp"))

ea_bnd$EACODE <- as.integer(ea_bnd$EACODE)

# District
dist_bnd <- st_read(here::here(
  "..", "PhD_geospatial-modelling", "data", "mwi-boundaries",
   "EN_NSO", "dist_bnd.shp"))




# Loading predicted mean
maize_se <- readRDS(here::here("data", "inter-output",
"maizeSe-mean-predicted.RDS"))

maize_se$EACODE <- as.integer(maize_se$EACODE)

maize_se <- maize_se %>% left_join(., ea_bnd)

head(maize_se)
sum(is.na(maize_se$maizeSe_mean))

# Loading the data
# Loading the plasma Se conc. dataset (cleaned from 01.cleaning-location.R)
plasma_se <- readRDS(here::here("data", "inter-output",
 "mwi-plasma-se_admin.RDS" )) # cleaned geo-loc maize Se data
# Checking co-location at EA level for plasma and maize
head(plasma_se)

plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
sum(duplicated(plasma_se$unique_id))
names(plasma_se)

# Generating a dataset w/ plasma Se & maize Se at admin (area) level
area  <- "EACODE"

maize_se  %>% filter(admin == area)  %>% 
 select(1, 5)  %>%  dplyr::rename_at(vars(admin_id), ~area)  %>% 
head()

test  <- plasma_se %>% select(-geometry)  %>% 
left_join(., maize_se %>% filter(admin == area)  %>% 
 select(1, 5)  %>%  dplyr::rename_at(vars(admin_id), ~area))

sum(is.na(test$maizeSe_mean))
# test  <- subset(test, !is.na(maizeSe_mean))

sum(duplicated(test$unique_id))
nrow(test)
names(test)
class(test)

test  <- left_join(test, dist_bnd)

geom  <- st_geometry(test$geometry)
st_geometry(test)  <- geom
class(test)

test  <- subset(test, !is.na(Plasma_Se))
test  <- subset(test, !is.na(wealth_quintile))
test  <- subset(test, !is.na(BMI))
test  <- subset(test, !is.na(had_malaria))
test  <- subset(test, !is.na(ANY_INFLAMMATION))
sum(is.na(test$ANY_INFLAMMATION))

test.adj <- poly2nb(test)
W.test <- nb2mat(test.adj, style = "B") 

test  <- test  %>% select(Plasma_Se, maizeSe_mean, wealth_quintile, BMI, urbanity,
is_smoker, had_malaria, ANY_INFLAMMATION, unique_id)
str(test)

test$wealth_quintile  <- as.factor(haven::zap_labels(test$wealth_quintile))
test$is_smoker  <- as.factor(haven::zap_labels(test$is_smoker))


# INLA for irregular lattice data 
# INLA model preps

# model to be fitted 
malawi.form <- log(Plasma_Se) ~ log(maizeSe_mean) +
wealth_quintile + BMI + urbanity + 
  had_malaria + ANY_INFLAMMATION

test$ID <- 1:nrow(test) # unique id for all cases (unique_id)

# random effect (w/o spatial corr.)
malawi.iid <- inla(update(malawi.form, . ~ . + f(ID, model = "iid")),
  data = as.data.frame(test),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
  return.marginals.predictor=TRUE),
  control.predictor = list(compute = TRUE))

summary(malawi.iid)

# Use 4 cores to process marginals in parallel
library("parallel")

options(mc.cores = 4)
# Transform marginals and compute posterior mean
#marginals: List of `marginals.fitted.values`from inla model
tmarg <- function(marginals) {
  post.means <- mclapply(marginals, function (marg) {
  # Transform post. marginals
  aux <- inla.tmarginal(exp, marg)
  # Compute posterior mean
  inla.emarginal(function(x) x, aux)
  })

  return(as.vector(unlist(post.means)))
}

# Add posterior means to the SpatialPolygonsDataFrame

test$IID <- tmarg(malawi.iid$marginals.fitted.values)

#Besag's improper
malawi.besag <- inla(update(malawi.form, . ~ . + f(ID, model = "besag",
 graph = W.test)),
  data = as.data.frame(test),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE),
  control.predictor = list(compute = TRUE))

summary(malawi.besag)

# convolution (BYM)
malawi.bym <- inla(update(malawi.form, . ~ . + f(ID, model = "bym", graph = W.test)),
  data = as.data.frame(test),
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, return.marginals.predictor=TRUE),
  control.predictor = list(compute = TRUE))

summary(malawi.bym)

par(mfrow = c(1, 2))
plot(test[, "maizeSe_mean"])
plot(test[, "Plasma_Se"])
