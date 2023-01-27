#####################################################################
#
# Libraries
#

library(psych)
library(Matrix)
library(numDeriv)
source("CEPHaStat_3.R")


######################################################################

data.df<-read.table("Se.dat",header=T)
#write.csv(data.df,"data.csv")
#data_f<-read.csv("data.csv",header=T)

names(data.df)

# Exploratory analysis of data 
#(to ensure reliability of variogram: checking for skewness) 

summa(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")])
summaplot(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")])

summa(log(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")]))
summaplot(log(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")]))

summa(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")])
summaplot(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")])

summa(log(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")]))
summaplot(log(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")]))


# Classified post-plot
#Checking for trends in data in order to determine the best model to use

quantiles<-as.character(cut(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")],
quantile(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")],
c(0,0.25,0.5,0.75,1)),include.lowest=T,
label=c("blue","green","yellow","red")))	

options(scipen=5)
plot(data.df$Easting[which(data.df$Crop=="Teff")],
data.df$Northing[which(data.df$Crop=="Teff")],
pch=16,col=quantiles,asp=1,
xlab="Longitude",ylab="Latitude",cex=0.5)

quantiles<-as.character(cut(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")],
quantile(data.df$Se_Crop_mgkg[which(data.df$Crop=="Wheat")],
c(0,0.25,0.5,0.75,1)),include.lowest=T,
label=c("blue","green","yellow","red")))	

options(scipen=5)
plot(data.df$Easting[which(data.df$Crop=="Wheat")],
data.df$Northing[which(data.df$Crop=="Wheat")],
pch=16,col=quantiles,asp=1,
xlab="Longitude",ylab="Latitude",cex=0.5)

plot(data.df$Temp[which(data.df$Crop=="Teff")],
log(data.df$Se_Crop_mgkg[which(data.df$Crop=="Teff")]),
pch=16,xlab="Temperature",ylab= "log Teff Se")

quantiles<-as.character(cut(data.df$Temp,
quantile(data.df$Temp,
c(0,0.25,0.5,0.75,1)),include.lowest=T,
label=c("blue","green","yellow","red")))	

options(scipen=5)
plot(data.df$Easting,
data.df$Northing,
pch=16,col=quantiles,asp=1,
xlab="Longitude",ylab="Latitude",cex=0.5)


# B.  Example 2.  Part-collocated data (Se in Teff and Predicted Se in Teff)


#########################################################################
#
#  z is vector of the variables that are treated as coregionalized
#  random variables.  Note that these are "stacked", so there would
#  be n1 + n2 + n3...+nm entries including primary and secondary variables
#  in the model.  Set n to n1 + n2 + n3...+nm
#

nobs<-nrow(data.df) #number of sample sites

z<-c(log(data.df$Se_Crop[which(data.df$CropCorrected=="Teff")]),data.df$pred_teff_se)
n<-length(z) #number of observations of all random variables (here = n_teff grain samples + nobs)

# number of coregionalized random variables in the model
# Se in Teff, Predicted Teff Se
m<-2  
varnames<-c("Se in Teff", "Predicted Teff Se")


#########################################################################
#
#  Make the design matrix M.  M has n rows.  The first m columns contain
#  dummy variables which indicate the variable (1 to m) which is included
#  in the corresponding entry in z.  At the same time we record the values
#  of n1, n2, ..., nm and check that they sum to n.
#

# Column 1 has constant (mean)for Teff Se 
 
M1<-vector("numeric",n)
M1[which(data.df$Crop=="Teff")]<-1

n1<-sum(M1)

# Column 2 has constant for Predicted Teff Se

M2<-c(rep(0,n1),rep(1,nobs))

n2<-sum(M2)

# Column 3 contains Temp, for Teff only (144 data points)
# Note we concatenate first the nobs separate observations of this covariate
# from the data file at the first n1 (Teff) sites, and then fill with zeroes

M3<-c(data.df$Temp[1:n1],rep(0,(n-n1))) 

# Column 4 contains Temp, for predicted Teff Se only

M4<-c(rep(0,n1),data.df$Temp) # 258 data points 


#M18<-c(rep(0,nobs),data.df$Temp)


M<-cbind(M1,M2,M3,M4)

#check by counting observations in each coregionalized variable

# NB: "n" is the number of observations of all random variables 
# which is n_teff grain samples + nobs
nv<-c(n1,n2)
n==sum(nv)#n<-length(z) 

##########################################################################

#Computing Distances between Observations

#  Make an n x n distance matrix corresponding to rows of M
#
# Put coordinates of locations into a matrix, then compute n x n
# distance matrix D
#


D<-matrix(nrow=n,ncol=n)
XY<-matrix(nrow=n,ncol=2)

#first put in coordinates for sites with Teff Se measurements
for (i in 1:n1) {
XY[i,1]<-data.df$Easting[i]
XY[i,2]<-data.df$Northing[i]
}

#now add in coordinates for additional collocated variable (predicted crop Se)

XY[(n1+1):n,1]<-data.df$Easting
XY[(n1+1):n,2]<-data.df$Northing

XM<-matrix(XY[,1],nrow=n,ncol=n)
YM<-matrix(XY[,2],nrow=n,ncol=n)


D<-sqrt(((XM-t(XM))^2)+((YM-t(YM))^2))
#write.csv(D,"ff.csv")
#
# Now make a m x m matrix Ci which indicates whether each combination
# of variables has collocated observations

Ci<-matrix(NA,m,m)
for (i in 1:m){
for (j in (i:m)){

if (i==1){nmini<-1} else {nmini<-sum(nv[1:(i-1)])+1}  #first index in z for observations in variable i
nmaxi<-sum(nv[1:i])

if (j==1){nminj<-1} else {nminj<-sum(nv[1:(j-1)])+1}  #first index in z for observations in variable i (j?)
nmaxj<-sum(nv[1:j])

if(min(D[nmini:nmaxi,nminj:nmaxj])==0){Ci[i,j]<-1
Ci[j,i]<-1
}else
{Ci[i,j]<-0
Ci[j,i]<-0
}
}
}

#
# Now make a Dirac matrix (1 where correlation is 1, 0 elsewhere) to manage
# nuggets

D0<-matrix(0,nrow=n,ncol=n)
D0[which(D==0)]<-1


#
#
###############################################################################
###############################################################################
#Estimating parameters by REML and GLS
#

describeBy(log(data.df$Se_Crop),data.df$CropCorrected)





#guess:  phi, log sd for each coregionalized variable nugget and
# correlated component, then nugget correlations (only for collocated variables),  
# spatially dependent correlations
#R12, R13, ... ,R1M, R2,3...


guess<-c(30,   #phi: distance parameter
-0.2,-0.2,	   #nugget variances (var1, var2) (log sd)
-0.8,-0.1,#correlated variances (var1, var2)(log sd)
0.5,     #Nugget cross-correlation
0.5)		   #cross-correlation of spatially correlated component 
corpar(0.5)

# set up vectors for profiles

Kap<-c(0.5,1.0,1.5,2.0) # smoothness parameter
prof<-c(0,0,0,0)

#selecting kappa (model) by rsl following Diggle and Ribeiro method

for (i in 1:5){
print(i)
kap<-Kap[i]
oo1<-optim(guess,rsl)
oo2<-optim(oo1$par,rsl)
prof[i]<-oo2$value
}


plot(Kap,prof,pch=16,xlab=expression(kappa),ylab="Negative log residual likelihood")
lines(Kap,prof,lty=3)

# estimating guess parameters by rsl using the selected model (kappa)
kap<-0.5
oo1<-optim(guess,rsl)
oo2<-optim(oo1$par,rsl)


#estimating fixed effects by a generalized least-squares
#constants for Teff Se and predicted Teff Se, and coefficients 
#for Temp for Teff Se and predicted Teff Se 
fec<-gls(oo2$par)
cbind(fec,fec[,1]/fec[,2])




########################################################################
#
# FUNCTIONS
#
########################################################################

# Residual log-likelihood following Marchant et al. 2009 notation
#
#
rsl<-function(theta){
#
#  First element in theta is the distance parameter phi
#
ph<-theta[1]

# Standard deviations of nugget term for each variable 
# (note optim uses the log)

s0<-vector("numeric",m)
s0<-theta[seq(2,(m+1))]
s0<-exp(s0)

# Standard deviations of correlated term for each variable 
# (note optim uses the log)

s1<-vector("numeric",m)
s1<-theta[seq((m+2),((2*m)+1))]
s1<-exp(s1)


# structural correlations.  There are up to m(m-1)/2 of these for
# nugget and exactly m(m-1)/2 for correlated component.  These are
# presented in theta in order
# R12, R13, ... ,R1M, R2,3...

# nugget correlations 

r0<-matrix(0,nrow=m,ncol=m)#  r0[u,v] is zero if there are no co-located 
#  observations of variables u and v.  Note all correlations are 
#  reparameterized so that optim is dealing with an unbounded value

diag(r0)<-1

nc<-(sum(Ci)-m)/2 #number of non-zero nugget structural correlations

# now modify r0 for any pairs of variables with collocated observations

rv0<-theta[seq(((2*m)+2),((2*m)+1+nc))] #pull out proposed values for non-zer0
#structural correlations
#reparameterize correlation parameters to values in [-1,1]

rv0<-unlist(lapply(rv0,corpar))

ico<-0
for(i in 1:(m-1)){
for(j in (i+1):m){
if(Ci[i,j]==1){
ico<-ico+1
r0[i,j]<-rv0[ico]
r0[j,i]<-r0[i,j]
}
}
}
ico<-0

# correlations for spatially dependent terms

nc1<-m*(m-1)/2
r1<-matrix(0,nrow=m,ncol=m)
diag(r1)<-1
rv1<-theta[seq(((2*m)+2+nc),length(theta))]
rv1<-unlist(lapply(rv1,corpar))

ico<-0
for (i in 1:(m-1)){
for (j in (i+1):m){
ico<-ico+1
r1[i,j]<-rv1[ico]
r1[j,i]<-rv1[ico]
}
}

#  Now calculate variogram matrices. To do this we loop over
# all combinations of coregionalized variables, and for each compute
# the corresponding submatrices of A11 and A01 from D and D0 and the
# parameters in s0, s1, r0, r1.

A<-matrix(NA,nrow=n,ncol=n)
N<-matrix(NA,nrow=n,ncol=n)
G<-matern(D,ph,kap)  # spatial correlation function (matrix)  given phi and kap and D

for (i in 1:m){
for (j in i:m){

if(i==1){start_i<-1}else{start_i<-sum(nv[1:(i-1)])+1}
end_i<-sum(nv[1:i])

if(j==1){start_j<-1}else{start_j<-sum(nv[1:(j-1)])+1}
end_j<-sum(nv[1:j])

A[start_i:end_i,start_j:end_j]<-
G[start_i:end_i,start_j:end_j]*s1[i]*s1[j]*r1[i,j]
A[start_j:end_j,start_i:end_i]<-
G[start_j:end_j,start_i:end_i]*s1[i]*s1[j]*r1[i,j]

#write.csv(A, "A.csv")

N[start_i:end_i,start_j:end_j]<-
D0[start_i:end_i,start_j:end_j]*s0[i]*s0[j]*r0[i,j]

N[start_j:end_j,start_i:end_i]<-
D0[start_j:end_j,start_i:end_i]*s0[i]*s0[j]*r0[i,j]
}
}

#write.csv(N, "N.csv")
#write.csv(V, "V.csv")

V<-N+A #covariance matrix from the matern function
if(min(eigen(V)$values)<0){
nrll<-Inf}else{
cholV<-chol(V)
logdetV<-2*sum(log(diag(cholV)))
Vi<-chol2inv(cholV)

Bhat<-(solve(t(M)%*%Vi%*%M))%*%(t(M)%*%Vi%*%z)

rankM<-rankMatrix(M)[1]

p1<-CM<-0.5*(n-rankM)*log(2*pi)-det(M%*%t(M))# Marchant et al 2009
p2<-0.5*logdetV
p3<-0.5*log(det(t(M)%*%Vi%*%M))
res<-(z-M%*%Bhat)
p4<-0.5*t(res)%*%Vi%*%res
nrll<-p2+p3+p4-p1
}
return(nrll)
}


###########################################################################
#
# extract generalized least squares estimates and their standard error
#

gls<-function(theta){

#
ph<-theta[1]

# Standard deviations of nugget term for each variable 
# (note optim uses the log)

s0<-vector("numeric",m)
s0<-theta[seq(2,(m+1))]
s0<-exp(s0)

# Standard deviations of correlated term for each variable 
# (note optim uses the log)

s1<-vector("numeric",m)
s1<-theta[seq((m+2),((2*m)+1))]
s1<-exp(s1)


# structural correlations.  There are up to m(m-1)/2 of these for
# nugget and exactly m(m-1)/2 for correlated component.  These are
# presented in theta in order
# R12, R13, ... ,R1M, R2,3...

# nugget correlations 

r0<-matrix(0,nrow=m,ncol=m)#  r0[u,v] is zero if there are no co-located 
#  observations of variables u and v.  Note all correlations are 
#  reparameterized so that optim is dealing with an unbounded value

diag(r0)<-1

nc<-(sum(Ci)-m)/2 #number of non-zero nugget structural correlations

# now modify r0 for any pairs of variables with collocated observations

rv0<-theta[seq(((2*m)+2),((2*m)+1+nc))] #pull out proposed values for non-zer0
			#structural correlations
			#reparameterize correlation parameters to values in [-1,1]

rv0<-unlist(lapply(rv0,corpar))

ico<-0
for(i in 1:(m-1)){
for(j in (i+1):m){
if(Ci[i,j]==1){
ico<-ico+1
r0[i,j]<-rv0[ico]
r0[j,i]<-r0[i,j]
}
}
}
ico<-0

# correlations for spatially dependent terms

nc1<-m*(m-1)/2
r1<-matrix(0,nrow=m,ncol=m)
diag(r1)<-1
rv1<-theta[seq(((2*m)+2+nc),length(theta))]
rv1<-unlist(lapply(rv1,corpar))

ico<-0
for (i in 1:(m-1)){
for (j in (i+1):m){
ico<-ico+1
r1[i,j]<-rv1[ico]
r1[j,i]<-rv1[ico]
}
}

#  Now calculate variogram matrices. To do this we loop over
# all combinations of coregionalized variables, and for each compute
# the corresponding submatrices of A11 and A01 from D and D0 and the
# parameters in s0, s1, r0, r1.

A<-matrix(NA,nrow=n,ncol=n)
N<-matrix(NA,nrow=n,ncol=n)
G<-matern(D,ph,kap)  # correlation matrix given phi and kap

for (i in 1:m){
for (j in i:m){

if(i==1){start_i<-1}else{start_i<-sum(nv[1:(i-1)])+1}
end_i<-sum(nv[1:i])

if(j==1){start_j<-1}else{start_j<-sum(nv[1:(j-1)])+1}
end_j<-sum(nv[1:j])

A[start_i:end_i,start_j:end_j]<-
G[start_i:end_i,start_j:end_j]*s1[i]*s1[j]*r1[i,j]
A[start_j:end_j,start_i:end_i]<-
G[start_j:end_j,start_i:end_i]*s1[i]*s1[j]*r1[i,j]

N[start_i:end_i,start_j:end_j]<-
D0[start_i:end_i,start_j:end_j]*s0[i]*s0[j]*r0[i,j]

N[start_j:end_j,start_i:end_i]<-
D0[start_j:end_j,start_i:end_i]*s0[i]*s0[j]*r0[i,j]
}
}


V<-N+A



cholV<-chol(V)
Vi<-chol2inv(cholV)

Bhat<-(solve(t(M)%*%Vi%*%M))%*%(t(M)%*%Vi%*%z) #Beta hat

C<-solve(t(M)%*%Vi%*%M)
op<-cbind(Bhat,sqrt(diag(C)))
colnames(op)<-c("Estimate","Standard error")
return(op)
}
###########################################################################

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


rsl_ndiff<-function(theta){

op<-theta
npar<-length(theta)

for (i in 1:npar){
del_theta<-theta
del_theta[i]<-theta[i]+(theta[i]*0.001)
op[i]<-(rsl(theta)-rsl(del_theta))/(theta[i]*0.001)
}
return(op)
}

###########################################################################
#
# convert correlation parameter to value in [-1,1]
#
corpar<-function(r){
return(exp(r)/(1+exp(r))*2-1)
}

############################################################################






