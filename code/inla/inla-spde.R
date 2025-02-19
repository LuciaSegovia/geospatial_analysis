# Cleaning the environment
rm(list = ls())

# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
#library(maptools) #spatial data manipulation
#library(rgeos)

# Loading the plasma Se conc. dataset (cleaned from 01.cleaning-location.R)
plasma_se <- readRDS(here::here("data", "inter-output",
                                "raw-maizeSe_plasma-se_ea.RDS" )) # cleaned geo-loc maize Se data

 
# Check tomorrow: for two random effects: 
# https://ourcodingclub.github.io/tutorials/inla/

# Check mean values == 0 in observed value (cluster)

# Checking co-location at EA level for plasma and maize
head(plasma_se)

# Renaming variable and checking indv. data
plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
sum(duplicated(plasma_se$unique_id))
names(plasma_se)

## Loading the data

file <- grep("plasma", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

plasma_se <- readRDS(here::here("data", "inter-output", "model", file[i]))

# Renaming variable and checking indv. data
plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
sum(duplicated(plasma_se$unique_id))
names(plasma_se)                       

# Removing values = 0 or NA
sum(plasma_se$Se_mean ==0)
sum(is.na(plasma_se$crp))

plasma_se <- plasma_se %>%
  select(Plasma_Se, wealth_quintile, BMI, urbanity,
        AGE_IN_YEARS, crp, agp,  unique_id, region, 
        survey_cluster1,  Latitude,  Longitude)

plasma_se <- na.omit(plasma_se)

#plasma_se <- plasma_se %>% filter(Se_mean != 0)

# First, preparing the data:

## Covariates selection (in a list) -----

#Check
# Change `had_malaria` for `Malaria_test_results`, and
# ANY_INFLAMATION for biomakers of infam: `crp` and `agp` 
# HIV check again

# Creating the mesh using the point data. (Not sure if it works)
# data.df <- plasma_se %>% 
#  select(Plasma_Se, wealth_quintile, BMI, urbanity,
#         is_smoker, Malaria_test_result, AGE_IN_YEARS,
#         crp, agp,  unique_id, region, survey_cluster1, # EACODE,
#         region, Latitude,  Longitude)


# var <- "Se_mean"
# 
# sum(is.na(plasma_se[, var]))
# class(plasma_se[, var])
# hist(plasma_se[, var])
# 
# plasma_se$maizeSe_mean[plasma_se$maizeSe_mean ==0]
# plasma_se$maizeSe_mean[plasma_se$maizeSe_mean ==0] <- NA
# min(plasma_se$maizeSe_mean[!is.na(plasma_se$maizeSe_mean)])
# plasma_se$maizeSe_mean[plasma_se$maizeSe_mean ==0] <- 0.002367136
# 
# plasma_se <- subset(plasma_se, !is.na(wealth_quintile) & !is.na(BMI) &
#                       !is.na(crp))

# Dataset for excluding urban WRA
# plasma_se <- subset(plasma_se, !is.na(wealth_quintile) & !is.na(BMI) &
  #                    !is.na(crp) & urbanity == "2")

#table(plasma_se$URBAN_RURA)
#dim(plasma_se)

# Assign values of covariates to points using value of nearest pixel
# excluding (for now) is_smoker, Malaria_test_result
covs <- plasma_se %>% select(wealth_quintile, BMI,  urbanity, 
                              Se_mean, 
                              AGE_IN_YEARS, crp, agp) %>% as.list()

# Intercept for spatial model
#covs$b0 <- rep(1, nv + n) # Becario precario
#covs$b0 <- rep(1, nrow(plasma_se)) # Similar to moraga


## INLA for point data

# Creating the mesh -----
# Mesh testing in mesh-testing.R

names(plasma_se)

coord <- cbind(plasma_se$Longitude, plasma_se$Latitude)

# Now compared with Krainski and Rue,  2017.
# https://inla.r-inla-download.org/r-inla.org/doc/vignettes/SPDEhowto.pdf
# Using the range as reference (from the variogram in "01_plasma-variogram.R")
# Range (downscale)
r <- 110/100
mesh <- inla.mesh.2d(coord, cutoff=r/10,
                     max.edge=c(r/4, r/2), offset=c(r/2, r))
plot(mesh, asp=1); points(coord, col='red')


# Getting the spatial term (s) ----

## Setting the SPDE model (Matern estimator) 
# (alpha is related to the smoothness parameter)


# No priors are set 
### From becario precario (7.3.3) & Moraga et al, 2021
spde0 <- inla.spde2.matern(mesh = mesh,
                          alpha = 2 ,
                          constr = TRUE) # this is optional

# Setting priors
# From: Krainski and Rue,  2017.
# https://inla.r-inla-download.org/r-inla.org/doc/vignettes/SPDEhowto.pdf
#Build the SPDE model on the mesh, where α = 3/2 for the exponential correlation
# and set the RF (Random Field) parameters prior distributions, [Fuglstad et al., 2017]

spde1 <- inla.spde2.pcmatern(
    mesh=mesh, alpha=1.5,
    prior.range=c(0.2, 0.5), ##P(range<0.2)=0.5 # Test instead c(1.1. 0.5) rest =
    prior.sigma=c(1, 0.5)) ## P(sigma>1)=0.5


## Setting the SPDE index (to store the random effect)
# Moraga called indexs
spde.index <- inla.spde.make.index(name = "spatial.field",
                                   n.spde = spde$n.spde)

# Trying to get the cluster as random effect (treating it as pseudo replication)
# Not working...
# mesh.index <- inla.spde.make.index(name = "spatial.field", 
#                                    n.spde = spde$n.spde, 
#                                    n.group = length(plasma_se$survey_cluster1))

# Projection matrix (A) obs.
A <- inla.spde.make.A(mesh = mesh , loc = coord)
Ap <- inla.spde.make.A(mesh = mesh , loc = coord) # Moraga (should be from pred. dataset)
#A.pred <- inla.spde.make.A(mesh = mesh, loc = coordinates(meuse.grid))

# Organise the data (stack)

# Create data structure
# stack <- inla.stack(data  = list(y = plasma_se$Plasma_Se),
#                           A = list(A, 1),
#                           effects = list(spde.index, covs),
#                           tag = "obs")

# IMPORTANT NOTE: order is important!!
stack <- inla.stack(data  = list(y = plasma_se$Plasma_Se),
                    A = list(A, 1),
                    effects = list(c(spde.index, list (Intercept = 1)),
                                  X=covs)
                   , 
                    tag = "obs")


# stk.e <- inla.stack(tag = "est" , 
#                     data = list(y = plasma_se$Plasma_Se,
#                     ids = plasma_se$survey_cluster1),
#                     A = list(1 , A),
#                     effects = list(spde.index, covs))

#Create data structure for prediction
# stack.pred <- inla.stack(data = list(y = NA),
#                                A = list(Ap, 1),
#                                effects = list(spde.index, covs),
#                                tag = "pred")

stack.pred <- inla.stack(data = list(y = NA),
                         A = list(Ap, 1),
                         effects = list(c(spde.index,
                                          list(Intercept =1)),
                                        covs),
                         tag = "pred")


# Join the stacks
join.stack <- inla.stack(stack, stack.pred)

#Fit model
# Becario
# form <- log(zinc) ~ -1 + Intercept + dist + f(spatial.field, model = spde)

# Moraga
#formula <− y ~ 0 + b0 + alt + temp + prec + hum + pop +
# aqua + f(s, model = spde)

#wealth_quintile, BMI, urbanity, maizeSe_mean, 
#AGE_IN_YEARS, crp, agp

# Plasma Se model
#form <- log(y) ~ 0 + b0 +  log(maizeSe_mean) +
#  wealth_quintile + BMI + #urbanity + #is_smoker + Malaria_test_result +
#   AGE_IN_YEARS +
#  log(crp) + log(agp) + f(spatial.field, model = spde)

form <- log(y) ~ -1 + Intercept +  log(Se_mean) +
  wealth_quintile + BMI +  urbanity + #is_smoker + Malaria_test_result +
  AGE_IN_YEARS +
  log(crp) + log(agp) + f(spatial.field, model = spde)

#form <- log(y) ~ -1 + Intercept +  log(Se_mean) +
#  wealth_quintile + BMI +  urbanity + #is_smoker + Malaria_test_result +
#  AGE_IN_YEARS +
#  log(crp) + log(agp) + f(spatial.field, model = spde,
#    control.group = list(model = "iid"))

# inla calculations

m1 <- inla(form, 
           data = inla.stack.data(stack),
family = "gaussian",
control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m1)

# inla calculations

m2 <- inla(form, 
           data = inla.stack.data(stack),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m2)


# inla calculations

m4 <- inla(form, 
           data = inla.stack.data(stack),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m4)

# inla calculations

m5<- inla(form, 
           data = inla.stack.data(stack),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m5)

# inla calculations

m9 <- inla(form, 
           data = inla.stack.data(stack),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m9)

# inla calculations

m10 <- inla(form, 
           data = inla.stack.data(stack),
           family = "gaussian",
           control.predictor = list(A = inla.stack.A(stack), compute = TRUE),
           control.compute = list(cpo = TRUE, dic = TRUE))

# Summary of results
summary(m10)

# Checking the residual spatial variation 

rang <-  apply(mesh$loc[, c(1,2)], 2 , range)
proj <- inla.mesh.projector(mesh, xlim = rang[ , 1],
                            ylim = rang[ , 2],
                            dims = c(200, 200)) 

mean_s <- inla.mesh.project(proj, 
                               m1$summary.random$spatial.field$mean)
sd_s <- inla.mesh.project(proj, 
                             m1$summary.random$spatial.field$sd)

df <- expand.grid(x = proj$x, y = proj$y)
df$mean_s <- as.vector(mean_s)
df$sd_s <- as.vector(sd_s)


# Mapping the spatial results
mean <-  ggplot(df, aes(x = x, y = y, fill = mean_s)) +
   geom_raster() +
  scale_fill_viridis_b(na.value = "transparent") +
  coord_fixed(ratio = 1) + theme_bw()

sd <-  ggplot(df, aes(x = x, y = y, fill = sd_s)) +
  geom_raster() +
  scale_fill_viridis_b(na.value = "transparent") +
  coord_fixed(ratio = 1) + theme_bw()

cowplot::plot_grid(mean, sd)


 



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


imat <- Diagonal(nv, rep(1, nv))

A.pp <-rbind(imat, lmat)


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


## More complex model (see ...)

## Setting the SPDE model (Matern estimator) 
# (alpha is related to the smoothness parameter)
# Using priors (range and sigma)

spde <- inla.spde2.pcmatern(mesh = mesh, 
                            alpha = 2,
                            prior.range = c(120, 0.9), # P(range < 120) = 0.9
                            prior.sigma = c(1, 0.01) # P(sigma > 10) = 0.01
)


lmat <- inla.spde.make.A(mesh, test_mesh) #becario


stack <- inla.stack(data = list(y = y.pp, e = e.pp),
                    A = list(A.pp, 1), 
                    effects = list(spde.index, covs),
                    tag = "pp")

## Following Claire script - Easy model test for spatial dependency
# Way better with spatial effect...
# Ex15_09C_IrishpH_INLA_V4.R - line 673

# Define sample size
N <- nrow(plasma_se)

StackFit <- inla.stack(tag = "obs", data = list(y = plasma_se$Plasma_Se),  
                       A = list(1, 1, A),                  
                       effects = list(   
                         Intercept = rep(1, N),
                         X = covs,
                         w = spde.index))



f2a <- log(y) ~ -1 + Intercept +  log(maizeSe_mean) +
  wealth_quintile + BMI + # urbanity + #is_smoker + Malaria_test_result +
  AGE_IN_YEARS +
  log(crp) + log(agp) 

f2b <- log(y) ~ -1 + Intercept +  log(maizeSe_mean) +
  wealth_quintile + BMI + # urbanity + #is_smoker + Malaria_test_result +
  AGE_IN_YEARS +
  log(crp) + log(agp) + f(spatial.field, model = spde)

# inla calculations


# First we run the model without spatial dependency.
IM2a <- inla(f2a,
             family = "gaussian", 
             data = inla.stack.data(StackFit),
             control.compute = list(dic = TRUE),
             control.predictor = list(A = inla.stack.A(StackFit)))
summary(IM2a)

# And this is the model with the spatial field
IM2b <- inla(f2b,
             family = "gaussian", 
             data=inla.stack.data(StackFit),
             control.compute = list(dic = TRUE),
             control.predictor = list(A = inla.stack.A(StackFit)))

summary(IM2b)
# Compare them
c(IM2a$dic$dic, IM2b$dic$dic)
