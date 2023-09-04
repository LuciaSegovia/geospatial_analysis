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

## INLA for point data

# Creating the mesh

data.df <- plasma_se %>% select(Plasma_Se, wealth_quintile, BMI, urbanity,
           is_smoker, had_malaria, Malaria_test_result, ANY_INFLAMMATION, unique_id, region, survey_cluster1,  EACODE,
           Latitude,  Longitude, geometry)

geom <- data.df$geometry

mesh2 <- inla.mesh.2d(loc = geom, max.edge = c(0.09, 0.3), 
                      cutoff = c(0.03))

names(plasma_se)

test_mesh  <- cbind(plasma_se$Longitude, plasma_se$Latitude)

min(test_mesh)
max(test_mesh)


mesh <- inla.mesh.2d(loc = test_mesh,  
offset = c(-0.1, 0.4),          
 max.edge = c(-20,1))


mesh2 <- inla.mesh.2d(loc = test_mesh, 
                      max.edge = c(72, 240),
                      cutoff = 12)


plot(mesh)

# Points (location) for INLA

# Points from the mesh
mesh.pts <- as.matrix(mesh$loc[,1:2])

#Combined with points from the actual datasets
all.pts <- rbind(mesh.pts, test_mesh)

#No of vertices is the mesh
nv <- mesh$n
nv2 <- mesh2$n

# No points in the data

n <-  nrow(test_mesh)

#Create SPDE
spde <- inla.spde2.pcmatern(mesh = mesh, alpha = 2,
                                prior.range = c(120, 0.9), # P(range < 120) = 0.9
                                prior.sigma = c(1, 0.01) # P(sigma > 10) = 0.01
)

# Generating the weights

library(deldir)
library(SDraw)
#install_github("tmcd82070/SDraw")

# Voronoi polygons (as SpatialPolygons)
mytiles <- voronoi.polygons(SpatialPoints(mesh$loc[, 1:2]))


#Compute weights (can take a while...)
require(rgeos)

w <- sapply(1:length(mytiles), function(p) {
  aux <- mytiles[p, ]  
  
  if(gIntersects(aux, malawi_bnd) ) {
    return(gArea(gIntersection(aux, malawi_bnd)))
  } else {
    return(0)
  }
})

plot(mytiles)

# Check sum of weights = area
sum(w)
gArea(malawi_bnd)

# Visual check

plot(mytiles, border ="gray")
points(mesh$loc[, 1:2], pch = 19, cex = 0.25)
lines(malawi_bnd, col = "green", lwd = 2)

#Prepare data
y.pp = rep(0:1, c(nv, n))
e.pp = c(w, rep(0, n))

lmat <- inla.spde.make.A(mesh, test_mesh)
imat <- Diagonal(nv, rep(1, nv))

A.pp <-rbind(imat, lmat)

spde.index <- inla.spde.make.index(name = "spatial.field",
                                       n.spde = spde$n.spde)

#Covariates

allpts.ppp <- ppp(all.pts[, 1], all.pts[, 2], owin(xrange = c(31.85, 36.97), # min and max of all.pts[,1]
                                                 yrange = c(-18.25, -8.36)))  # min and max of all.pts[,2]

# Assign values of covariates to points using value of nearest pixel

covs <- plasma_se %>% select(Plasma_Se, wealth_quintile, BMI, urbanity,
                     is_smoker, had_malaria, ANY_INFLAMMATION) %>% as.list()


# Intercept for spatial model
covs$b0 <- rep(1, nv + n)


#Create data structure
stack <- inla.stack(data = list(y = y.pp, e = e.pp),
                        A = list(A.pp, 1), 
                        effects = list(spde.index, covs),
                        tag = "pp")

pred <-  inla.stack(data = list(y = NA),
                    A = list(1), 
                    effects = list(c(spde.index,
                          list(intercept = 1))), tag = "pred")

pp.res0 <- inla(y ~ 1 + 
                  f(spatial.field, model = spde), 
                family = "gaussian", data = stack,
                control.predictor = list(A = stack, compute = TRUE,
                                         link = 1),
                control.inla = list(int.strategy = "eb"),
                E = stack$e)


# the mwi bnd can be obtained in (00_cleaning-boundaries.R)
mwi.mesh   <- inla.mesh.2d(boundary = malawi_bnd, 
cutoff = 0.1,
 offset = c(1, 0.5), 
 max.edge = c(0.1, 0.5))

plot(mwi.mesh)


