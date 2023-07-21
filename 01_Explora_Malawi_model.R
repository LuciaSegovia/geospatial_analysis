
#  Malawi, grain nutrient concentrations modelled with (i) environmental covariates
#  or (ii) soil properties.
#
#  The analyses on the Malawi data are done using the geoR package.  This is 
#  because the data set is much larger than in Ethiopia, which is computationally
#  demanding, and also, because Malawi is narrow from East to West the coordinates
#  can be projected onto the Universal Transverse Mercator projection for a single
#  zone.  This allows the computations to be done more rapidly.
#
#  This script is for maize Se concentration, as with the Ethiopian analysis
#  each variable may have a different sequence of models fitted, depending
#  on whether the predictors are accepted or rejected at each stage.
#
#############################################################################

library(maps)
library(mapdata) 
library(sp)
library(geoR)
source(here::here("CEPHaStat_3.R"))

#############################################################################

data.df  <- readRDS(here::here("data", "inter-output","mwi-maize-se.RDS")) # cleaned geo-loc maize Se data

# Checking missing values: 2 pH
sum(is.na(data.df$BIO1))
data.df[which(is.na(data.df$pH)),]
data.df  <- subset(data.df, !is.na(pH)) # removing NA


##########################################################################
#  
#  Analyses with soil properties
#

#  Exploratory analysis

# Se conc. vs pH
par(mfrow=c(1,2))
par(mar=c(5,5,3,3))
plot(data.df$pH,data.df$Se_mg,pch=16,xlab="Soil pH",
ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")

# Not using Organic matt. 
#par(mar=c(5,5,3,3))
#plot(data.df$Org_pct,data.df$Se_triplequad,pch=16,xlab="Soil organic carbon/ %",
#ylab=expression("Maize grain Se/ mg kg"^{-1}),log="xy")

mod<-lm(log(Se_mg)~pH,data=data.df)

summa(mod$residuals)
summaplot(mod$residuals)

#Not using precip.
#par(mfrow=c(2,2))
#par(mar=c(5,5,2,2))
#plot(data.df$BIO12,data.df$Se_triplequad,pch=16,xlab="Mean annual precipitation /mm",
#ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")

# Se conc. vs MAT (downscaled/10)
par(mar=c(5,5,2,2))
plot((data.df$BIO1)/10,data.df$Se_mg,pch=16,xlab="Mean annual temperature /°C",
ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")

#par(mar=c(5,5,2,2))
#plot((data.df$TIM),data.df$Se_triplequad,pch=16,xlab="Topographic Index",
#ylab=expression("Maize grain Se/ mg kg"^{-1}),log="y")
#

mod<-lm(log(Se_mg)~BIO1,data=data.df)

summary(mod)
anova(mod)
summa(mod$residuals)
summaplot(mod$residuals)


mod<-lm(log(Se_mg)~pH+BIO1,data=data.df)

summa(mod$residuals)
summaplot(mod$residuals)
summary(mod)
anova(mod)

# There seems to be a corelation between pH & BIO1. 
#REVIEW: Maybe dropping BIO1? If there is colinearity?

# Collinearity
cor(data.df$pH,  data.df$BIO1, use = "complete.obs")
cor(log(data.df$Se_mg),  data.df$BIO1, use = "complete.obs")
cor(log(data.df$Se_mg),  data.df$pH, use = "complete.obs")

###########################################################################
#
#  Project the data onto the UTM zone 36S
#
UTM36S="+init=epsg:32736"

locs<-cbind(data.df$Longitude,data.df$Latitude)
loc_sp<-SpatialPoints(locs, proj4string=CRS("+proj=longlat +datum=WGS84"))
loc_UTM<- spTransform(loc_sp, CRS(UTM36S))


#  Create a dataframe with the UTM coordinates, grain nutrient conc.
#  and pH (soil property) & MAT (enviromental covariates)

gdata<-data.frame(cbind(loc_UTM@coords,data.df$Se_mg,
data.df$pH,data.df$BIO1))

#
#  Next, make a geodata object for the geoR library procedures
#

geod<-as.geodata(gdata,covar.col = c(4,5))

#
#  Fit a null model in which the only fixed effect is a constant mean
#

mod0<-likfit(geod,
trend="cte",cov.model="exponential",ini.cov.pars=c(1,30.0),lambda=0,
lik.method="ML")

#Extract the maximized log-likelihood for the null model

l0<-mod0$loglik  #2892.015

mod0$parameters.summary
mod0$model.components
mod0$practicalRange

# Next, include the first soil property (pH) as a proposed predictor
mod1<-likfit(geod,
trend=~V4,cov.model="exponential",ini.cov.pars=c(1,30.0),lambda=0,
lik.method="ML")

#Extract the maximized log-likelihood

l1<-mod1$loglik  #2905.181

mod1$parameters.summary

# Compute the log-likelihood ratio statistic for adding pH, and the p-value
# for the null hypothesis,

L1<-2*(l1-l0)  #26.22 
pval<-1-pchisq(L1,1) #p=2.87 x 10^-7

#Extract the fixed effects coefficients
mod1$beta
#  intercept     covar1 
#-4.9157879  0.2256435

#Extract the standard error of the coefficient for pH

sqrt(mod1$beta.var[2,2]) #0.04350672


#  Next add MAT as a potential covariate

mod2<-likfit(geod,
trend=~V4+V5,cov.model="exponential",ini.cov.pars=c(1,30.0),lambda=0,
lik.method="ML")

#Extract the maximized log-likelihood

l2<-mod2$loglik  #2910.369

# Compute the log-likelihood ratio statistic for adding MAT, and the p-value
# for the null hypothesis,

L2<-2*(l2-l1)  # 10.37673
pval<-1-pchisq(L2,1) #p=0.001276137

#  Next model Downscaled mean annual temperature (MAT) as the only covariate
#
#emod2<-likfit(geod,
#trend=~V5,cov.model="exponential",ini.cov.pars=c(1,30.0),lambda=0,
#lik.method="ML")
#
## Extract the maximized log-likelihood 
#
#el2<-emod2$loglik  #2901.194
#
## Compute the log-likelihood ratio statistic for adding MAP, and the p-value
## for the null hypothesis,
#
#Le2<-2*(el2-l0)  #18.35844
#pval<-1-pchisq(Le2,1) #1.830061e-05
#
# We have evidence to retain MAP as a predictor