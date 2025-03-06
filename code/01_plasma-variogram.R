# Cleaning the environment
rm(list = ls())

# Loading libraries and functions

library(dplyr) # data wrangling
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("functions", "CEPHaStat_3.R")) #stat functions
# Skewness[-1,+1], Octile skewness [−0.2,0.2]
library(geoR)  # geospatial modelling
library(geosphere) # spatial functions for variogram
library(gstat) # variogram function

## Loading data ----

# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
plasma.df  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
  filter(!is.na(selenium)) # %>% select(1:48) # removing buffer and other spatial vars

names(plasma.df)

data.df <- plasma.df %>% dplyr::select(selenium, Latitude, Longitude)

hist(data.df$selenium)
summaplot(data.df$selenium)
summaplot(log(data.df$selenium))


######################################################################################################################
#
#  Exploratory study of empirical variogram
#
#  Make a variogram cloud
#

N<-nrow(data.df)


NP<-0.5*N*(N-1) 

Long<-data.df$Longitude
Lat<-data.df$Latitude
#z<-data.df$selenium
z<-log(data.df$selenium)

lag<-vector("numeric",NP)
vclo<-vector("numeric",NP)
bear<-vector("numeric",NP)

ico=0

for (i in 1:(N-1)){
  for (j in (i+1):N){
    ico=ico+1
    print(ico/NP)
    lag[ico]<-distVincentySphere(c(Long[i],Lat[i]),c(Long[j],Lat[j]))/1000 #Default meters, changing to km
    bear[ico]<-finalBearing(c(Long[i],Lat[i]),c(Long[j],Lat[j]), a=6378137, f=1/298.257223563, sphere=TRUE)
    zi<-(z[i])
    zj<-(z[j])
    vclo[ico]<-(zi-zj)
  }
}

lagbins<-cut(lag,seq(0,200,10),labels=seq(1,200,10))   # 10-km bins for Malawi

lag2<-lag[!is.na(lagbins)]
vclo2<-vclo[!is.na(lagbins)]
lagbins<-factor(lagbins[!is.na(lagbins)])
nlags<-nlevels(lagbins)

bear<-bear[!is.na(bear)]

# Checking distance
min(lag2)
max(lag2)/2 # Km
max(bear)

####################################################

# Form estimates of the variogram

# Matrices in which to keep, for the lag bins in lagbins:

#  i. semiv - variogram estimates, fifth column is isotropic Matheron, columns
#  1 to 4 are directional (Matheron estimator), columns 6 and 7 are isotropic
#  estimates with Cressie-Hawkins and Dowd estimators respectively.
#
#  ii. npair and varlags - respectively the number of lag pairs per bin, and the mean
#	lag distance.  Columns 1 to 4 are directional (as in semiv) and 5 is isotropic.

semiv<-matrix(nrow=nlags,ncol=7)
npair<-matrix(nrow=nlags,ncol=5)
varlags<-matrix(nrow=nlags,ncol=5)


colnames(semiv)<-c("N-S,Ma","NE-SW,Ma","W-E,Ma","SE-NW,Ma","iso-Ma","iso-CH","iso-Do")
colnames(varlags)<-c("N-S","NE-SW","W-E","SE-NW","iso")
colnames(npair)<-c("N-S","NE-SW","W-E","SE-NW","iso")


varlags[,5]<-as.numeric(by(lag2, lagbins, mean))
npair[,5]<-as.numeric(by(lag2, lagbins, sum))/as.numeric(by(lag2, lagbins, mean))

#
# estimator
#
#  Matheron
semiv[,5]<-0.5*(as.numeric(by((vclo2)^2, lagbins, mean)))

# CH
semiv[,6]<-(as.numeric(by((sqrt(abs(vclo2))), lagbins, mean)))^4
semiv[,6]<-0.5*semiv[,6]/(0.457+0.459/npair[,5]+0.045/npair[,5]^2)

# Do

vclo2d<-c(vclo2,-vclo2)
lagbinsd<-c(lagbins,lagbins)

semiv[,7]<-0.5*(as.numeric(by(vclo2d, lagbinsd, mad)))^2

####################################################

#  Now plot the variograms

maxv<-max(semiv[,5:7])
minv<-min(semiv[,5:7])

plot(varlags[,5],semiv[,5],ylim=c(0,maxv),xlab="Distance /km",ylab="Variance",pch=16)
#plot(varlags[,5],semiv[,6],ylim=c(0,maxv),xlab="Distance /km",ylab="Variance",pch=16)
points(varlags[,5],semiv[,6],pch=1, col ="blue")
points(varlags[,5],semiv[,7],pch=17,col = "red")
abline(v=110)

locsv<-minv*c(0.6,0.4,0.2)
points(60,locsv[1],pch=16)
points(60,locsv[2],pch=1 , col ="blue")
points(60,locsv[3],pch=17, col = "red")

text(65,locsv[1],"Matheron",pos=4)
text(65,locsv[2],"Cressie-Hawkins",pos=4)
text(65,locsv[3],"Dowd",pos=4)


# Variogram (gstat) -----
#https://r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html

coordinates(data.df) <- ~Longitude+Latitude
proj4string(data.df) <- CRS("+init=epsg:4326")

# Variogram and fit variogram
v <- variogram(log(selenium) ~1, data.df)
fit.vgm <- fit.variogram(v, vgm("Exp"))
fit.variogram(v, vgm(c("Exp", "Sph")), fit.kappa = TRUE)

plot(v)
abline(v=110)

m = 100 
points = matrix(runif(m*2),m,2)

head(points)
