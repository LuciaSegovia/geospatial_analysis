
# Set INLA repository 
#options(repos = c(getOption("repos"),
#   INLA="https://inla.r-inla-download.org/R/stable"))
#
#   # Install INLA and dependencies (from CRAN)
#install.packages("INLA", dep = TRUE)

#repos <- c(CRAN = "https://cloud.r-project.org", INLA = "https://inla.r-inla-download.org/R/stable")
#options(repos = repos)

# Loading libraries and functions

library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(rgdal)
library(INLA) # Modelling
library(gstat) # variogram
library(geosphere) # calculating distance between points. 
source(here::here("CEPHaStat_3.R"))


# Loading the datat
EligibleDHS  <- readRDS(here::here("data","inter-output","dhs.rds")) 
GPS <- st_read(here::here("data", "MWGE7AFL", "MWGE7AFL.shp")) #GPS location DHS
GPS<-dplyr::rename(GPS, survey_cluster1='DHSCLUST')

# add GPS values
EligibleDHS <- merge(EligibleDHS, GPS, by='survey_cluster1')
EligibleDHS<-dplyr::rename(EligibleDHS, latitude='LATNUM')
EligibleDHS<-dplyr::rename(EligibleDHS, longitude='LONGNUM')
EligibleDHS<-dplyr::rename(EligibleDHS, altitude_in_metres='ALT_GPS')

# Removing entries w/o Se

EligibleDHS  <- subset(EligibleDHS, !is.na(selenium))

dim(EligibleDHS)
#names(EligibleDHS)

sum(duplicated(EligibleDHS$unique_id))
length(unique(EligibleDHS$survey_cluster1))
class(EligibleDHS$wealth_quintile)
class(EligibleDHS$sdist)

EligibleDHS$wealth_quintile  <- haven::zap_labels(EligibleDHS$wealth_quintile)

plot(EligibleDHS[, "wealth_quintile"])
table(EligibleDHS$wealth_quintile, EligibleDHS$region)

# BMI and selenium first before doing anything
plot(x = EligibleDHS$BMI, 
     y = log(EligibleDHS$selenium))

EligibleDHS$selenium[is.na(EligibleDHS$BMI)]
# Collinearity
cor(EligibleDHS$BMI,  EligibleDHS$selenium, use = "complete.obs")

# Collinearity
cor(EligibleDHS$BMI,  EligibleDHS$wealth_quintile, use = "complete.obs", 
method = "kendall")

# Collinearity
cor(EligibleDHS$selenium,  EligibleDHS$Malaria_test_result, use = "complete.obs", 
method = "kendall")

sum(is.na(EligibleDHS$selenium))


summaplot(log(EligibleDHS$selenium))

# Checking the covariates
sum(is.na(EligibleDHS$wealth_quintile))

data.df  <- subset(EligibleDHS, !is.na(wealth_quintile)) # Remoing NA in wealth

# Wealth Q
mod<-lm(log(selenium)~wealth_quintile,data=data.df)

summa(mod$residuals)
summaplot(mod$residuals)
summary(mod)
anova(mod)

# We are including it
sum(is.na(EligibleDHS$BMI))
data.df  <- subset(EligibleDHS, !is.na(wealth_quintile) & !is.na(BMI)) # Remoing NA in wealth

# BMI 
mod<-lm(log(selenium)~wealth_quintile+BMI,data=data.df)

summa(mod$residuals)
summaplot(mod$residuals)
summary(mod)
anova(mod)

# Not including it

# We are including it
sum(is.na(EligibleDHS$Malaria_test_result))
data.df  <- subset(EligibleDHS, !is.na(wealth_quintile) & !is.na(Malaria_test_result)) # Remoing NA in wealth

# Malaria 
mod<-lm(log(selenium)~wealth_quintile+Malaria_test_result,data=data.df)

summa(mod$residuals)
summaplot(mod$residuals)
summary(mod)
anova(mod)

# Not including it

# Se conc. vs MAT (downscaled/10)
par(mar=c(5,5,2,2))
plot(data.df$wealth_quintile,log(data.df$selenium),
pch=16,xlab="Wealth Q",
ylab=expression("Plasma Se/ mg ml"^{-1}),log="y")
abline(h=mean(log(data.df$selenium)))

# # Is the quality of the data good enough
# to include interactions in the model? No -
#Lowerdepth

 ggplot(data = data.df) + geom_point(
                    aes(y = log(selenium) , x = wealth_quintile),
                    shape = 1, 
                    size = 1) + 
                    xlab("Wealth Q") + ylab("Selenium")+ 
                    theme(text = element_text(size=15)) +
                     geom_smooth(data = data.df, 
                     aes(x = wealth_quintile, 
                         y = selenium),
                     method = "lm") +
                      facet_wrap(.~ sdist,ncol = 5)


#########################################################
# We will fit the following model in INLA:
# pH_i ~ N(mu_i, sigma^2)
# E[pH_i] = mu_i
# mu_i = Covariate stuff
# We will use all main terms, 2-way interactions and 
# the 3-way interaction.

# Also calculate fitted values; hence the control.predictor
# line.
I1 <- inla(selenium ~ wealth_quintile,
           family = "gaussian",
           control.predictor = list(compute = TRUE),
           data = EligibleDHS)

summary(I1)

# Numerical output for the betas
Beta1 <- I1$summary.fixed[, c("mean", "sd", 
                              "0.025quant", 
                              "0.975quant")] 
print(Beta1, digits = 2)

# Numerical output for the sigma. Remember that INLA
# works with tau = 1 / sigma^2. We are not interested
# in the tau. We want the sigma. We need the INLA
# conversion functions to convert output from tau 
# to sigma.
MySqrt <- function(x) {1 / sqrt(x) }
tau <- I1$marginals.hyperpar$`Precision for the Gaussian observations`
sigma <- inla.emarginal(MySqrt, tau)
sigma


# Model validation
Fit1 <- I1$summary.fitted.values[,"mean"]
E1   <- EligibleDHS$selenium - Fit1


#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit1, y = E1)
abline(h = 0, v = 0)


boxplot(E1 ~ wealth_quintile, data = EligibleDHS)
abline(h = 0)


boxplot(selenium ~ wealth_quintile, data = EligibleDHS)
abline(h = 0)

# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data

data.df  <- data.df %>% dplyr::select( selenium, survey_cluster1,
                                       Longitude, Latitude) %>% 
  filter(!is.na(selenium))

head(data.df)

# Distance (Vincenty Sphere)
points  <-  data.df %>% dplyr::select(Longitude, Latitude)

dist_mat <- distm(points, fun = distVincentySphere)  # Apply distm function
dist_mat <- dist_mat/1000 

sum(dist_mat<50)
sum(dist_mat>120)

# Variogram  - changing coordinates lon/lat to nor/east

#cord.dec  =  SpatialPoints(cbind(GPS$LONGNUM, -GPS$LATNUM), proj4string = CRS("+proj=longlat"))
cord.dec  =  SpatialPoints(cbind( GPS$LONGNUM, GPS$LATNUM), proj4string = CRS("+proj=longlat"))
cord.dec  =  SpatialPoints(cbind( EligibleDHS$longitud, EligibleDHS$latitud), proj4string = CRS("+proj=longlat"))
cord.dec  =  SpatialPoints(cbind( data.df$Longitude, data.df$Latitude), proj4string = CRS("+proj=longlat"))



# Transforming coordinate to UTM using EPSG=32736 for WGS 84 / UTM zone 36S
# found on https://epsg.io/32736
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32736"))
cord.UTM

# Plotting points
par(mfrow = c(1, 2))
plot(cord.dec, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)





# Loading the data
# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
data.df  <- readRDS(here::here("data", "inter-output","dhs-gps.rds")) # cleaned geo-loc plasma Se data

data.df  <- data.df %>% dplyr::select( selenium, survey_cluster1, urbanity, 
                                       Longitude, Latitude) %>% 
  filter(!is.na(selenium))



# This is how you make a sample variogram  

MyData <- data.frame(selenium =data.df$selenium, 
                     Xkm = cord.UTM$coords.x1/1000, 
                     Ykm = cord.UTM$coords.x2 /1000)

coordinates(MyData) <- c("Xkm", "Ykm")

V1 <- variogram(selenium ~ Xkm + Ykm , 
                data = MyData, 
                cressie = TRUE,
                cutoff = 400)
plot(V1, 
     xlab = list(label = "Distance", cex = 1.5),
     ylab = list(label = "Cressie's semivariance", cex = 1.5),
     col = 1, pch = 16, smooth = TRUE) 
