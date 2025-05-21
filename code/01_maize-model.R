###########################################################################
###########################################################################
#
#  R script for exploratory analysis, variogram estimation and validation
#  and kriging of grain nutrient concentrations
#
###########################################################################
###########################################################################

# Libraries & functions

library(geosphere)
source(here::here("functions", "CEPHaStat_3.R")) # stat functions
source(here::here("functions", "kriging-functions.R")) # Matern functions & others

#  Note that the script contains an adaptation of the scripts develop for 
# the analysis of the Malawi  and the Ethiopia data sets (GeoNutrition)

###################################################################
#
#  Read in the maize data set
#

# maize <- read.csv(here::here("data", "maize", "Malawi_Grain.csv"))
# maize$Se_triplequad [maize$Se_triplequad >9000] <- NA

# min(maize$Se_triplequad)
#maize_se  <- readRDS(here::here("data", "inter-output","mwi-maize-se.RDS")) # cleaned geo-loc maize Se data
data.df  <- readRDS(here::here("data", "inter-output", "mwi-grain-se_raw.RDS")) # cleaned geo-loc maize Se data (2 datasets)
names(data.df)


## Choosing only entries with maize Se values
data.df <-  subset(data.df, Crop == "Maize")
# Housekeeping: Check NA and zeros
sum(is.na(data.df$Se_raw))
data.df <- subset(data.df, !is.na(Se_raw)) #excluding NA
sum(data.df$Se_raw == 0)
min(data.df$Se_raw[data.df$Se_raw>0]) 
sum(data.df$Se_raw == min(data.df$Se_raw[data.df$Se_raw>0]))
# Changing zeros to min (for log-transformation)
# data.df$Se_raw[data.df$Se_raw == 0] <- min(data.df$Se_raw[data.df$Se_raw>0])
# Removing zeros
data.df <- subset(data.df, Se_raw >0) # Excluding zeros


# Now create plot name with Se (element of interest)

element<-"Se_raw"
#Crop.target <- "maize"
plotname<-expression("Se (raw) /"~mg~kg^{-1})
plotname2<-expression("Se (raw) /"~log~mg~kg^{-1})


# Compute summary statistics of the selected variable

data.df  <- as.data.frame(data.df)

summary<-(summa(data.df[,element]))
summary<-rbind(summary,(summa(log(data.df[,element]))))
row.names(summary)<-c("mg/kg,","log mg/kg")

summaplot((data.df[,element]),plotname)
summaplot(log(data.df[,element]),plotname2)
summaplot((outliers(data.df[,element],trim=T)))
summaplot((outliers(log(data.df[,element]),trim=T)))
summa((outliers(data.df[,element],trim=T)))
summa((outliers(log(data.df[,element]),trim=T)))

# At this point a decision is made whether to analyse the data on their
# original units or to replace them with their natural logarithms.
# In this study the Se was transformed to logs
#
#
#  The next two commands are commented out so that they are not 
#  automatically run
#
  data.df[,element]<-log(data.df[, element]) 
  plotname<-plotname2

# The following two rows will remove outliers
# defined as values outwith the outer fences as in Tukey (1977)
# Exploratory Data Analysis, Addison-Wesley.
# This was done for Ethiopia Ca Teff data

#ol<-outliers(data.df[,1])
#no_out_data.df<-data.df[-ol,]
  
# ## Exploring the location of the outliers (see if it could be spatially aggregated)
#  plot(data.df[-ol,"Longitude"],data.df[-ol,"Latitude"], # Lon & Lat variables
#       pch=16,asp=1,
#       xlab="Longitude",ylab="Latitude",cex=0.5, add = TRUE)
#  
## Exploring the plot w/o the outliers
# plot(data.df[ol,"Longitude"],data.df[ol,"Latitude"], # Lon & Lat variables
#       pch=16,asp=1, col="red", 
#       xlab="Longitude",ylab="Latitude",cex=0.5, add = TRUE)
#
# Produce a classified post-plot of the data with the bottom
# quartile in blue, the next in green, the next in yellow and
# the top in red.
#

quantiles<-as.character(cut(data.df[,element],quantile(data.df[,element],
c(0,0.25,0.5,0.75,1)),include.lowest=T,
label=c("blue","green","yellow","red")))	

## SM- Fig. 2a -----
# Classified post-plot
dev.new()
options(scipen=5)
plot(data.df[,"Longitude"],data.df[,"Latitude"], # Lon & Lat variables
pch=16,col=quantiles,asp=1,
xlab="Longitude",ylab="Latitude",cex=0.5)

locsv<-c(-16.2,-16.5,-16.8, -17.1)
cols <- c("blue","green","yellow","red")
points(30,locsv[1],pch=16, col=cols[1])
points(30,locsv[2],pch=16, col=cols[2])
points(30,locsv[3],pch=16, col=cols[3])
points(30,locsv[4],pch=16, col=cols[4])

text(30.2,locsv[1],"Lowest Quantile (0-25%)",pos=4)
text(30.2,locsv[2],"Mid-lower Quantile (25-50%)",pos=4)
text(30.2,locsv[3],"Mid-higher Quantile (50-75%)",pos=4)
text(30.2,locsv[4],"Highest Quantile (75-100%)",pos=4)

# Exploratory plots to show E-W or N-S trends

par(mfrow=c(1,2))
par(mar=c(5,5,3,3))
plot(data.df[,"Longitude"],(data.df[,element]),pch=16,cex=0.8,
xlab="Longitude",ylab=plotname)

plot(data.df[,"Latitude"],(data.df[,element]),pch=16,cex=0.8,
xlab="Latitude",ylab=plotname)


######################################################################################################################
#
#  Exploratory study of empirical variogram
#
#  Make a variogram cloud
#

N<-nrow(data.df) # No. of observations

NP<-0.5*N*(N-1)   # No. of pairs

# Extracting variables from the data set
Long<-data.df$Longitude # Lon
Lat<-data.df$Latitude # Lat
z<-data.df[,element] # maize Se conc.

# Generating vectors to store the data to be calculated (loop below)
lag<-vector("numeric",NP) # Distance between 2pts (lag)
vclo<-vector("numeric",NP) # variogram cloud (diff. between two obs.)
bear<-vector("numeric",NP) # Shortest distance between 2pts (on an ellipsoid or sphere)

ico=0

for (i in 1:(N-1)){
for (j in (i+1):N){
ico=ico+1
print(ico/NP)
lag[ico]<-distVincentySphere(c(Long[i],Lat[i]),c(Long[j],Lat[j]))/1000 #
bear[ico]<-finalBearing(c(Long[i],Lat[i]),c(Long[j],Lat[j]), a=6378137, f=1/298.257223563, sphere=TRUE)
zi<-(z[i])
zj<-(z[j])
vclo[ico]<-(zi-zj)
}
}

# Setting for the variogram:
# max. distance (e.g., end of spatial correlation) around 100km (around half of max. distance in Malawi)
lagbins<-cut(lag,seq(0,100,10),labels=seq(5,100,10))   # 10-km bins for Malawi

lag2<-lag[!is.na(lagbins)]
vclo2<-vclo[!is.na(lagbins)]
lagbins<-factor(lagbins[!is.na(lagbins)])
nlags<-nlevels(lagbins)

####################################################

# Form estimates of the variogram

# Matrices in which to keep, for the lag bins in lagbins:

#  i. semiv - variogram estimates, fifth column is isotropic Matheron, columns
#  1 to 4 are directional (Matheron estimator), columns 6 and 7 are isotropic
#  estimates with Cressie-Hawkins and Dowd estimators respectively. - Excluded directional 
#
#  ii. npair and varlags - respectively the number of lag pairs per bin, and the mean
#	lag distance.  Columns 1 to 4 are directional (as in semiv) and 5 is isotropic. - Only isotropic

#semiv<-matrix(nrow=nlags,ncol=7)
semiv<-matrix(nrow=nlags,ncol=3)
#npair<-matrix(nrow=nlags,ncol=5)
npair<-matrix(nrow=nlags,ncol=1)
#varlags<-matrix(nrow=nlags,ncol=5)
varlags<-matrix(nrow=nlags,ncol=1)


#colnames(semiv)<-c("N-S,Ma","NE-SW,Ma","W-E,Ma","SE-NW,Ma","iso-Ma","iso-CH","iso-Do")
colnames(semiv)<-c("iso-Ma","iso-CH","iso-Do")
#colnames(npair)<-c("N-S","NE-SW","W-E","SE-NW","iso")
colnames(npair)<-c("iso")
#colnames(varlags)<-c("N-S","NE-SW","W-E","SE-NW","iso")
colnames(varlags)<-c("iso")

varlags[,1]<-as.numeric(by(lag2, lagbins, mean))
npair[,1]<-as.numeric(by(lag2, lagbins, sum))/as.numeric(by(lag2, lagbins, mean))

#
# estimator
#
#  Matheron
semiv[,1]<-0.5*(as.numeric(by((vclo2)^2, lagbins, mean)))

# CH
semiv[,2]<-(as.numeric(by((sqrt(abs(vclo2))), lagbins, mean)))^4
semiv[,2]<-0.5*semiv[,2]/(0.457+0.459/npair[,1]+0.045/npair[,1]^2)

# Do

vclo2d<-c(vclo2,-vclo2)
lagbinsd<-c(lagbins,lagbins)

semiv[,3]<-0.5*(as.numeric(by(vclo2d, lagbinsd, mad)))^2

####################################################

## SM- Fig. 2b -----

#  Now plot the variograms

maxv<-max(semiv[,1:3])
minv<-min(semiv[,1:3])

plot(varlags[,1],semiv[,1],ylim=c(0,maxv),xlab="Distance /km",ylab="Variance",pch=16)
points(varlags[,1],semiv[,2],pch=1)
points(varlags[,1],semiv[,3],pch=17)

locsv<-minv*c(0.6,0.4,0.2)
points(60,locsv[1],pch=16)
points(60,locsv[2],pch=1)
points(60,locsv[3],pch=17)

text(65,locsv[1],"Matheron",pos=4)
text(65,locsv[2],"Cressie-Hawkins",pos=4)
text(65,locsv[3],"Dowd",pos=4)


#  Fit exponential variogram functions to each set of estimates ----

# Reasonable initial estimates of the parameters must be entered below

co<-20000
c1<-30000
a<-50

maxl<-max(varlags[,1])*1.1
par(mfrow=c(2,2))

# Matheron's estimator

h<-varlags[,1]
sv<-semiv[,1]
npa<-npair[,1]
kap<-0.5

oo<-optim(c(co,c1,a),wls
,method="L-BFGS-B",
lower=c(0.0,0.0,0),
upper=c(1000000,1000000,5000))

Ahat<-length(h)*log(oo$value)+6  

ooMa<-oo

oon<-optim(c(120),wlsnugg
,method="L-BFGS-B",
lower=c(0.0),
upper=c(1000000))

Ahatn<-length(h)*log(oon$value)+2  

plot(varlags[,1],semiv[,1],ylim=c(0,maxv),xlim=c(0,maxl),xlab="Distance /km",ylab="Variance",pch=16)
#lines(seq(0,200),rep(oon$par[1],201))
lines(seq(0,maxl,0.1),oo$par[1]+oo$par[2]*(1-exp(-seq(0,maxl,0.1)/oo$par[3])))

mtext("a). Matheron",3,adj=0,line=0.5)

# Cressie's and Hawkins's estimator  CH

h<-varlags[,1]
sv<-semiv[,2]
npa<-npair[,1]
kap<-0.5

oo<-optim(c(co,c1,a),wls
,method="L-BFGS-B",
lower=c(0.0,0.0,0),
upper=c(1000000,1000000,5000))

Ahat<-length(h)*log(oo$value)+6  

ooCH<-oo

oon<-optim(c(120),wlsnugg
,method="L-BFGS-B",
lower=c(0.0),
upper=c(1000000))

Ahatn<-length(h)*log(oon$value)+2  

# Plotting the data
plot(varlags[,1],semiv[,2],ylim=c(0,maxv),xlim=c(0,maxl),xlab="Distance /km",ylab="Variance",pch=16)
lines(seq(0,maxl,0.1),oo$par[1]+oo$par[2]*(1-exp(-seq(0,maxl,0.1)/oo$par[3])))
mtext("b). Cressie-Hawkins",3,adj=0,line=0.5)


# Dowd's estimator Do


h<-varlags[,1]
sv<-semiv[,3]
npa<-npair[,1]
kap<-0.5

oo<-optim(c(co,c1,a),wls
,method="L-BFGS-B",
lower=c(0.0,0.0,0),
upper=c(1000000,1000000,5000))
ooDo<-oo
Ahat<-length(h)*log(oo$value)+6  

oon<-optim(c(120),wlsnugg
,method="L-BFGS-B",
lower=c(0.0),
upper=c(1000000))

Ahatn<-length(h)*log(oon$value)+2  

plot(varlags[,1],semiv[,3],ylim=c(0,maxv),xlim=c(0,maxl),xlab="Distance /km",ylab="Variance",pch=16)
lines(seq(0,maxl,0.1),oo$par[1]+oo$par[2]*(1-exp(-seq(0,maxl,0.1)/oo$par[3])))
mtext("c). Dowd",3,adj=0,line=0.5)

#
#
#  Cross-validation, performed on a subset of the data (all in Ethiopia, 1000 in Malawi)
#
#  An exploratory plot of prediction errors is produced for each variogram
#  Validation outputs are saved in arrays kvopMA, kvopCH and kvopDo for the
#  estimators of Matheron, Cressie & Hawkins and Dowd respectively.
#  

# Check CI 
#Nv<-N # no need to subset for Ethiopia
Nv<-500 # vs 1000 Malawi - check perc. ()



kvop<-matrix(nrow=Nv,ncol=5)


Nr<-Nv-1

# Make random sample without replacement

sam<-sample(1:N,Nv,replace=F)
#
# Subsample data
Long_s<-Long[sam]
Lat_s<-Lat[sam]
z_s<-(z[sam])

#
# Compute distance matrix for subsample

DM<-matrix(0,nrow=Nv,ncol=Nv)

for (i in 1:(Nv-1)){
for (j in (i+1):Nv){
DM[i,j]<-distVincentySphere(c(Long_s[i],Lat_s[i]),c(Long_s[j],Lat_s[j]))/1000 #WGS84 ellipsoid is used by default
DM[j,i]<-DM[i,j]
}
}

#
#Select parameter set to validate

ooX<-ooMa  # Matheron

# variogram matrix

Gam<-ooX$par[1]+ooX$par[2]*(1-matern(DM,ooX$par[3],0.5))
diag(Gam)<-0

for (i in 1:Nv){

print(i)

A<-Gam[-i,-i]
A<-cbind(A,rep(1,Nr))
A<-rbind(A,c(rep(1,Nr),0))

z_0<-z_s[i]
z_n<-z_s[-i]

b<-c(Gam[i,-i],1)

lam<-solve(A)%*%b
Zhat<-sum(z_n*lam[1:Nr])
err<-z_0-Zhat
kv<-t(b)%*%lam
theta<-(err^2)/kv

kvop[i,]<-c(z_0,Zhat,err,kv,theta)
}

dev.new()
summaplot(kvop[,3],"Kriging error")
kvopMa<-kvop


#Select parameter set to validate

ooX<-ooCH

# variogram matrix

Gam<-ooX$par[1]+ooX$par[2]*(1-matern(DM,ooX$par[3],0.5))
diag(Gam)<-0

for (i in 1:Nv){

print(i)

A<-Gam[-i,-i]
A<-cbind(A,rep(1,Nr))
A<-rbind(A,c(rep(1,Nr),0))

z_0<-z_s[i]
z_n<-z_s[-i]

b<-c(Gam[i,-i],1)

lam<-solve(A)%*%b
Zhat<-sum(z_n*lam[1:Nr])
err<-z_0-Zhat
kv<-t(b)%*%lam
theta<-(err^2)/kv

kvop[i,]<-c(z_0,Zhat,err,kv,theta)
}

dev.new()
summaplot(kvop[,3],"Kriging error")
kvopCH<-kvop


#Select parameter set to validate

ooX<-ooDo

# variogram matrix

Gam<-ooX$par[1]+ooX$par[2]*(1-matern(DM,ooX$par[3],0.5))
diag(Gam)<-0

for (i in 1:Nv){

print(i)

A<-Gam[-i,-i]
A<-cbind(A,rep(1,Nr))
A<-rbind(A,c(rep(1,Nr),0))

z_0<-z_s[i]
z_n<-z_s[-i]

b<-c(Gam[i,-i],1)

lam<-solve(A)%*%b
Zhat<-sum(z_n*lam[1:Nr])
err<-z_0-Zhat
kv<-t(b)%*%lam
theta<-(err^2)/kv

kvop[i,]<-c(z_0,Zhat,err,kv,theta)
}

dev.new()
summaplot(kvop[,3],"Kriging error")
#summa(kvop[,5])
kvopDo<-kvop

colnames(kvopMa)<-c("z_0","Zhat","err","kv","theta")
colnames(kvopCH)<-c("z_0","Zhat","err","kv","theta")
colnames(kvopDo)<-c("z_0","Zhat","err","kv","theta")

#######################################################
#
#  Print out mean and median standardized squared prediction
#  error for each variogram.  If the median for Matheron
#  is in the interval [thetaCLL,thetaCLU] calculated below
#  then it is selected.  Otherwise the variogram for which
#  the median is in the range and closest to 0.455
#

# Print out mean and median standardized squared prediction
#  error for each variogram.
summa(kvopMa[,5])[,c(1,2)]
summa(kvopCH[,5])[,c(1,2)]
summa(kvopDo[,5])[,c(1,2)]

# Interval calculation [thetaCLL,thetaCLU] 
thetaCLU<-0.455+2*sqrt(1/(8*(Nv/2)*(dchisq(0.455,1))^2))
thetaCLL<-0.455-2*sqrt(1/(8*(Nv/2)*(dchisq(0.455,1))^2))

print(c(thetaCLL,thetaCLU))

#Select parameter set for kriging (from ooMa, ooCH, ooDo depending
# on the decision above

#ooX<-ooCH #non-log
#ooX<-ooMa  #log
ooX<-ooDo  #log, BLOD no zero.


# Writing output prior to kriging -----

Models<-rbind(ooMa$par,ooCH$par,ooDo$par)
colnames(Models)<-c("Co","C1","A")
rownames(Models)<-c("Ma","CH","Do")

# Fitted variograms
fname<-paste0("data/OK/",paste(Sys.Date(),element,Crop.target,"fitted_variograms.dat", sep="_"))
write.table(Models,fname,quote=F)

#renaming variables before saving 
colnames(varlags)<-c("varlag")
colnames(varlags)<-c("npair")
colnames(semiv)<-c("semivar_Ma","semivar_CH","semivar_Do")

# Variogram estimates
fname<-paste0("data/OK/",paste(Sys.Date(), element,Crop.target,"variogram_estimates.dat", sep="_"))
write.table(cbind(varlags,npair,semiv),fname,quote=F,row.names=F)

# Cross-validation Matheron
fname<-paste0("data/OK/",paste(Sys.Date(),element,Crop.target,"XVal_Ma.dat",sep="_"))
write.table(kvopMa,fname,quote=F,row.names=F)

#Cross-validation Cressie & Hawkins
fname<-paste0("data/OK/",paste(Sys.Date(), element,Crop.target,"XVal_CH.dat",sep="_"))
write.table(kvopCH,fname,quote=F,row.names=F)

# Cross-validation Dowd
fname<-paste("data/OK/",paste(Sys.Date(), element,Crop.target,"XVal_Do.dat",sep="_"))
write.table(kvopDo,fname,quote=F,row.names=F)

# cross-valiation sub-sample
fname<-paste0("data/OK/",paste(Sys.Date(),element,Crop.target,"XVal_sample.dat",sep="_"))
write.table(sam,fname,quote=F,row.names=F)

# Summary stat table
fname<-paste0("data/OK/",paste(Sys.Date(),element,Crop.target,"_summary.dat",sep="_"))
write.table(summary,fname,quote=F)

#####################################################################
#
#  Kriging
#

#targets.df<-read.csv("Ethiopia_map.csv",header=T) #Option for Ethiopia
targets.df<-read.csv(here::here("data", "maize", "Malawi_targets.csv"),header=T) # Option for Malawi

urdata.df<-data.frame(cbind(Lat,Long,z))

# Make a distance matrix among all data points 

Long<-urdata.df$Long
Lat<-urdata.df$Lat

DM<-matrix(0,nrow=N,ncol=N)

for (i in 1:(N-1)){
for (j in (i+1):N){

DM[i,j]<-distVincentySphere(c(Long[i],Lat[i]),c(Long[j],Lat[j]))/1000 #WGS84 ellipsoid is used by default
DM[j,i]<-DM[i,j]
}
}

# variogram matrix

Gam<-ooX$par[1]+ooX$par[2]*(1-matern(DM,ooX$par[3],0.5))

A<-Gam
A<-cbind(A,rep(1,N))
A<-rbind(A,c(rep(1,N),0))
Ainv<-solve(A)

# No of unsampled obs.
Nt<-nrow(targets.df)

# Matrix to store the data from the OK
# Note we added one more column bc its lognormal OK (see docu)
#krop<-matrix(nrow=Nt,ncol=4)
krop<-matrix(nrow=Nt,ncol=5)

# Binding all the sampled locations
Allpoints<-cbind(Lat,Long)

# OK prediction

for (it in 1:Nt){
print(c(it,Nt))

# Extract Lat and Long of target point 

Lat_t<-targets.df$Lat[it]
Long_t<-targets.df$Long[it]

Bd<-matrix(nrow=(N+1),ncol=1)

for (j in 1:N){
Bd[j]<-distVincentySphere(c(Long[j],Lat[j]),c(Long_t,Lat_t))/1000
}

#maxdis<-max(Bd[1:Ne])
#print(c(it,it/Nt,maxdis))

Bd[1:N]<-ooX$par[1]+ooX$par[2]*(1-matern(Bd[1:N],ooX$par[3],0.5))
Bd[N+1]<-1
lam<-Ainv%*%Bd
Zhat<-sum(z*lam[1:N])
kv<-t(Bd)%*%lam
lagr<-lam[N+1]
krop[it,]<-c(Long_t,Lat_t,Zhat,kv,lagr)
}

colnames(krop)<-c("Longitude","Latitude","Zhat","kv", "lagr")

fname<-paste("data/OK/",paste(Sys.Date(), element,"_OK_",Crop.target,".csv",sep=""))

write.csv(krop,fname,row.names=F)  

krop <- data.frame(krop)

# Back-transforming lognormal OK
krop$Zhat_exp <- exp(krop$Zhat + krop$kv/2 - krop$lagr)

# Saving exponential OK
fname<-paste("data/OK/",paste(Sys.Date(), element,"_OK_exp",Crop.target,".csv",sep=""))

write.csv(krop,fname,row.names=F)  

