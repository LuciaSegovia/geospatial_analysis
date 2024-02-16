##################################################################################
#
#  Libraries
#

library(rgdal)
library(geoR)
library(geosphere)


#
##############################################################################
#
#  District boundaries 
#
##############################################################################
#
# Read in shape file

dist.shp<-readOGR(dsn=".",layer="mwi_admbnda_adm2_nso_hotosm_20230405")


dist.shp@data$ADM2_EN

# These are in Lat and Long

# Plot 

plot(dist.shp) 
lines(coordinates(dist.shp@polygons[[1]]@Polygons[[1]]),col="red") 

ndist<-28

dist.indx<-(seq(1:32))[-c(7,17,31,32)] #exclude city districts
dist.names<-dist.shp@data$ADM2_EN[dist.indx]
##############################################################################

##############################################################################
#
#  Read in grain data
#
##############################################################################

read_in.df<-read.csv("malawi.csv",header=T)

# Now remove any rows which are not maize

read_in.df<-read_in.df[which(read_in.df$Crop=="Maize"),]

# Now extract columns of interest: Latitude, Longitude, and analyses for
# elements of interest ( Ca, Co, Cu, Fe, I, Mg, Mn, Mo, Se, Zn)
# (based on MRB email of 22/4/20).
#
#  Analysis for a specified element (from Ca, Co, Cu, Fe, I, Mg, Mn, Mo, 
# Se_triplequad, Zn)
# (based on MRB email of 22/4/20).
#

element<-"Zn"

element.col<-which(colnames(read_in.df)==element)
data.df<-read_in.df[,c(3,4,element.col)]



# Extract missing values [for present, if >=88888, these codes still require
# explanation

missing.rows<-which(data.df[,3]>=77777.0)
if(length(missing.rows)>0) {data.df<-data.df[-missing.rows,]}


Np<-nrow(data.df)

z<-data.df[,3]

# convert coordinates to UTM 36-S

xy<-data.df[,c(2,1)]
names(xy)<-c("LONG","LAT")
coordinates(xy)<-~LONG+LAT
proj4string(xy)<-CRS("+proj=longlat +datum=WGS84")
UTM = "+init=epsg:20936"
rectsr<- spTransform(xy, CRS(UTM))
data.df[,1:2]<-rectsr@coords


#############################################################################
#
# Set up variogram model
#
#############################################################################


pars<-read.table("Zn fitted_variograms.dat",header=T)[1,]


#######
#
#  Now loop over all districts
#
#######
#
#  Matrix to store output


krop<-matrix(NA,nrow=ndist,ncol=4)
colnames(krop)<-c("Easting","Northing","Block_mean","KV")


for (iwo in 1:ndist){
print(c(iwo,ndist))
W.index<-dist.indx[iwo]

##############################################################################
#
# Make discretization of specified district
#
##############################################################################

#First, count the constituent polygons

npol<-length(dist.shp@polygons[[W.index]]@Polygons)

# Now extract the points along the boundary of the selected district into xy

W.grid<-data.frame(matrix(c(0,0),nrow=1))
colnames(W.grid)<-c("Var1","Var2")

for (ipol in 1:npol){

if(dist.shp@polygons[[W.index]]@Polygons[[1]]@hole=="FALSE"){
xy<-data.frame(coordinates(dist.shp@polygons[[W.index]]@Polygons[[ipol]]))
names(xy)<-c("LONG","LAT")
coordinates(xy)<-~LONG+LAT
proj4string(xy)<-CRS("+proj=longlat +datum=WGS84")
UTM = "+init=epsg:20936"
locs<-data.frame((spTransform(xy, CRS(UTM)))@coords)# metres relative to UTM datum
colnames(locs)<-c("Easting","Northing")


boundary<-cbind(locs$Easting,locs$Northing)

#
#  Make a square grid inside the boundary 1500-m interval
P.grid<-locations.inside(pred_grid(boundary,by=1500),boundary)
if(length(P.grid)>1){
W.grid<-rbind(W.grid,P.grid)
}
}
}
W.grid<-W.grid[-1,]


#... then make it into a SpatialPoints set


dp<-data.frame(W.grid)
Ndisc<-nrow(dp)

krop[iwo,1]<-mean(dp[,1])
krop[iwo,2]<-mean(dp[,2])

Easting<-c(data.df[,1],dp[,1])
Northing<-c(data.df[,2],dp[,2])


# SET UP DISTANCE MATRIX

Nv<-Np+Ndisc


DM<-as.matrix(dist(c(Easting,Northing),diag=T,upper=T))/1000

# Convert Distance Matrix to matrix of variogram values Gam

Gam<-as.numeric(pars[1])+as.numeric(pars[2])*(1-matern(DM,as.numeric(pars[3]),0.5))
diag(Gam)<-0


dispVar<-mean(Gam[(Np+1):Nv,(Np+1):Nv]) # compute the within-district dispersion matrix

A<-rbind(Gam[1:Np,1:Np],rep(1,Np))      # compute A matrix for the BK equation
A<-cbind(A,rep(1,(Np+1)))
A[(Np+1),(Np+1)]<-0

b<-vector("numeric",(Np+1))		# compute augmented vector of point to block variogram values, b
for(j in 1:Np){
b[j]<-mean(Gam[j,(Np+1):Nv])
}
b[(Np+1)]<-1

lam<-solve(A)%*%b			# solve for weights and Lagrangian
krest<-sum(z*lam[1:Np])			# compute BK prediction
kvar<-t(b)%*%lam-dispVar		# compute BK kriging variance
krop[iwo,3]<-krest
krop[iwo,4]<-kvar
}

Krop<-data.frame(krop)
Krop$District<-dist.names

write.csv(Krop,"Zinc_block_kriging.csv",quote=F,row.names=F)


# Rough map

cscale<-(krop[,3]-19)/(26-19)

cols<-c(rep("lightgreen",90),rep("darkgreen",103))



pal<-colorRampPalette(c("ivory","green4"))(15)

cscale<-round(2*(krop[,3]-19),0)+1
par(mar = c(1, 1,3, 0.1))
plot(dist.shp,col=pal[cscale])


#########################################################################
#
# FUNCTIONS
#
#########################################################################

matern<-function (u, phi, kappa) 
{
    if (is.vector(u)) 
        names(u) <- NULL
    if (is.matrix(u)) 
        dimnames(u) <- list(NULL, NULL)
    uphi <- u/phi
    uphi <- ifelse(u > 0, (((2^(-(kappa - 1)))/ifelse(0, Inf, 
        gamma(kappa))) * (uphi^kappa) * besselK(x = uphi, nu = kappa)), 
        1)
    uphi[u > 600 * phi] <- 0
    return(uphi)
}

