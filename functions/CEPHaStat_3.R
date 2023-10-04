#############################################################################
#
#  CEPHaStat:  R functions produced for the CEPHaS GCRF Project, Working 
#	Group 4 for use in CEPHaS and related research on conservation 
#	agriculture
#
#

header<-"\n\nCEPHaStat is a collection of R functions produced by the UKRI GCRF
CEPHaS project.  The functions relate primarily to statistical analysis of
soil and related data from agricultural and environmental surveys and 
experiments.  The CEPHaStat file can be shared as it stands, and is made 
available to all for use, without warranty or liability.\n\n"
#
fnclist<-"Current list of functions\n
skew kurt ocskew
summa summaplot outliers
okgridvar, sv, dis
vanG, vanGdraw, campB, campDdraw, plotgroups\n \n"

vers<-3
#############################################################################

cat(paste("CEPHaStat version ",vers))
cat(header)
#cat(fnclist)
#############################################################################
#############################################################################
#
# SUMMARY STATISTICS
#
#

skew<-function(x){

# compute coefficient of skewness of data in x

x<-na.drop(x)
n<-length(x)
xd<-x-mean(x)
mu3<-sum(xd^3)/(n-1)
mu<-sqrt(sum(xd^2)/(n-1))
sk<-mu3/(mu^3)
return(sk)
}

#############################################################################

kurt<-function(x){

# compute coefficient of kurtosis of data in x

x<-na.drop(x)
n<-length(x)
xd<-x-mean(x)
mu4<-sum(xd^4)/(n-1)
mu<-sqrt(sum(xd^2)/(n-1))
sk<-(mu4/(mu^4))-3
return(sk)
}

################################################################################

ocskew<-function(x){

# compute the octile skewness of values in x

x<-na.drop(x)
Ocs<-quantile(x,c(1/8,0.5,7/8))
os<-((Ocs[3]-Ocs[2])-(Ocs[2]-Ocs[1]))/(Ocs[3]-Ocs[1])
return(os)
}

###########################################################################

summa<-function(x,sigf){

# compute summary statistics of values in x

if(missing(sigf)){rosig<-F}else{rosig<-T}


x<-na.drop(x)
Q1<-quantile(x,prob=0.25)
Q3<-quantile(x,prob=0.75)
Q2<-quantile(x,prob=0.5)
hspread<-Q3-Q1
Fu<-Q3+3*hspread
Fl<-Q1-3*hspread

# ols,oll: values below and above outer fences
# posols,posoll: values below and above inner fences 
# (so ols and posols overlap, as do oll and posoll
#

ols<-which(x<Fl)
oll<-which(x>Fu)
posols<-which(x<(Q1-1.5*hspread))
if(length(posols)==0){
lw<-min(x)}else{
lw<-min(x[-posols])}
posoll<-which(x>(Q3+1.5*hspread))
if(length(posoll)==0){
uw<-max(x)}else{
uw<-max(x[-posoll])}

ol<-c(ols,oll) # combined outlier set

nol<-length(ol)

outp<-matrix(c(mean(x),Q2,Q1,Q3,var(x),sqrt(var(x)),skew(x),ocskew(x),kurt(x),nol),1,10)

if(rosig=="TRUE"){outp<-signif(outp,sigf)}

colnames(outp)<-c("Mean","Median",
"Quartile.1", "Quartile.3","Variance","SD","Skewness",
"Octile skewness","Kurtosis",
"No. outliers")


return(outp)

}

####################################################################

summaplot<-function(x,varname){

# plot a histogram with boxplot and QQ plot of data in x indicating
# any probable outliers by Tukey's criterion

x<-na.drop(x)
if(missing(varname)){varname<-"x"}

Q1<-quantile(x,prob=0.25)
Q3<-quantile(x,prob=0.75)
Q2<-quantile(x,prob=0.5)
hspread<-Q3-Q1
Fu<-Q3+3*hspread
Fl<-Q1-3*hspread


# ols,oll: values below and above outer fences
# posols,posoll: values below and above inner fences 
# (so ols and posols overlap, as do oll and posoll
#
ols<-which(x<Fl)
oll<-which(x>Fu)
posols<-which(x<(Q1-1.5*hspread))
if(length(posols)==0){
lw<-min(x)}else{
lw<-min(x[-posols])}
posoll<-which(x>(Q3+1.5*hspread))
if(length(posoll)==0){
uw<-max(x)}else{
uw<-max(x[-posoll])}

ol<-c(ols,oll) # combined outlier set
par(mfrow=c(1,2))
ymax<-max((hist(x,plot=F))$counts)
hist(x,main="",col="AliceBlue", xlab=varname,ylim=c(0,(ymax*1.25)))
means<-mean(x)

boxmin<-ymax*1.1
boxmax<-ymax*1.2
boxmid<-ymax*1.15

lines(c(Q1,Q3),c(boxmin,boxmin))
lines(c(Q1,Q3),c(boxmax,boxmax))
lines(c(Q1,Q1),c(boxmin,boxmax))
lines(c(Q3,Q3),c(boxmin,boxmax))
lines(c(Q1,lw),c(boxmid,boxmid))
lines(c(Q3,uw),c(boxmid,boxmid))
lines(c(Q2,Q2),c(boxmin,boxmax),lwd=2)
points(c(means), c(boxmid),col="red",pch=16)

lines(c(Fu,Fu),c(10,boxmid),lty=5,col="red")
lines(c(Fl,Fl),c(10,boxmid),lty=5,col="red")

qqn<-qqnorm(x,main="",pch=16)
qqline(x)
points(qqn$x[ol],qqn$y[ol],pch=16,col="red")

}
####################################################################

histplot<-function(x,varname){

# plot a histogram with boxplot in x indicating
# any probable outliers by Tukey's criterion

x<-na.drop(x)
if(missing(varname)){varname<-""}

Q1<-quantile(x,prob=0.25)
Q3<-quantile(x,prob=0.75)
Q2<-quantile(x,prob=0.5)
hspread<-Q3-Q1
Fu<-Q3+3*hspread
Fl<-Q1-3*hspread


# ols,oll: values below and above outer fences
# posols,posoll: values below and above inner fences 
# (so ols and posols overlap, as do oll and posoll
#
ols<-which(x<Fl)
oll<-which(x>Fu)
posols<-which(x<(Q1-1.5*hspread))
if(length(posols)==0){
lw<-min(x)}else{
lw<-min(x[-posols])}
posoll<-which(x>(Q3+1.5*hspread))
if(length(posoll)==0){
uw<-max(x)}else{
uw<-max(x[-posoll])}

ol<-c(ols,oll) # combined outlier set

ymax<-max((hist(x,plot=F))$counts)
hist(x,main="",col="AliceBlue", xlab=varname,ylim=c(0,(ymax*1.25)))
means<-mean(x)

boxmin<-ymax*1.1
boxmax<-ymax*1.2
boxmid<-ymax*1.15

lines(c(Q1,Q3),c(boxmin,boxmin))
lines(c(Q1,Q3),c(boxmax,boxmax))
lines(c(Q1,Q1),c(boxmin,boxmax))
lines(c(Q3,Q3),c(boxmin,boxmax))
lines(c(Q1,lw),c(boxmid,boxmid))
lines(c(Q3,uw),c(boxmid,boxmid))
lines(c(Q2,Q2),c(boxmin,boxmax),lwd=2)
points(c(means), c(boxmid),col="red",pch=16)

lines(c(Fu,Fu),c(10,boxmid),lty=5,col="red")
lines(c(Fl,Fl),c(10,boxmid),lty=5,col="red")

}
###########################################################################

outliers<-function(x,trim){

# compute summary statistics of values in x

if(missing(trim)){trim<-F}

x<-na.drop(x)
Q1<-quantile(x,prob=0.25)
Q3<-quantile(x,prob=0.75)
Q2<-quantile(x,prob=0.5)
hspread<-Q3-Q1
Fu<-Q3+3*hspread
Fl<-Q1-3*hspread

# ols,oll: values below and above outer fences
# posols,posoll: values below and above inner fences 
# (so ols and posols overlap, as do oll and posoll
#

ols<-which(x<Fl)
oll<-which(x>Fu)
posols<-which(x<(Q1-1.5*hspread))
if(length(posols)==0){
lw<-min(x)}else{
lw<-min(x[-posols])}
posoll<-which(x>(Q3+1.5*hspread))
if(length(posoll)==0){
uw<-max(x)}else{
uw<-max(x[-posoll])}

ol<-c(ols,oll) # combined outlier set

nol<-length(ol)

print(paste(nol," value(s) lie outwith the outer fences (Tukey, 1977)"),quote=F)

if(nol!=0){
print(paste("Outlying values are:"),quote=F)
print(x[ol])
print(paste("Indices of outlying values are"),quote=F)

if(trim==F){
return(ol)}else{
print(ol)
return(x[-ol])
}
}
}

###########################################################################

cor.sig<-function(x,roc=3,roP=3){
  
  require(Hmisc)

  data<-rcorr(as.matrix(x))
  p_value<-round(data$P,roP)
  cor_table<-round(data$r,roc)

  data<-paste(cor_table, sep="")
  data <- matrix(data, ncol=ncol(x))
  diag(data) <- paste(diag(cor_table), " ", sep="")
  data[lower.tri(data, diag = TRUE)] <- ""
  data<-as.vector(data)

  data1<-paste(p_value, sep="")
  data1 <- matrix(data1, ncol=ncol(x))
  diag(data1) <- paste(diag(cor_table), " ", sep="")
  data1[upper.tri(data1, diag = TRUE)] <- ""
  data1<-as.vector(data1)
  
  data2 <- matrix(paste(data, data1, sep=""), ncol=ncol(p_value))
  rownames(data2) <- colnames(p_value)
  colnames(data2)<-colnames(p_value)
  data2<-as.data.frame(data2)
  print(data2)
  }

###############################################################
###############################################################

qqnorm.line<-function(x,col="black"){
  qqnorm(x)
  qqline(x,col=col)
}



###############################################################
###############################################################

okgridvar<-function(space,modtype,c0,c1,a1){

x0<-c(0,0)
x<-rep(seq(-2.5,2.5,1),6)*space
y<-rep(seq(-2.5,2.5,1),rep(6,6))*space
xg<-cbind(x,y)

a<-matrix(1,37,37)
a[37,37]<-0
b<-rep(1,37)

for (i in 1:36){
lag<-ddis(i,xg)
b[i]<-sv(lag,modtype,c0,c1,a1)
for (j in i:36){
if(i==j){a[i,j]<-0.0
}else{
lag<-dis(i,j,xg)
a[i,j]<-sv(lag,modtype,c0,c1,a1)
a[j,i]<-a[i,j]}
}
}

ai<-solve(a)
lam<-ai%*%b
kv<-t(b)%*%lam
return(kv)
}

#####################################################################


ossfim<-function(mod,spmin,spmax,L,wrout){

if(missing(wrout)){wrout<-FALSE}

#  Evaluate kriging variances for grids of different spacing

# Nugget variance, correlated variance and distance parameter

c0<-mod$nugget
c1<-mod$cov.pars[1]
a1<-mod$cov.pars[2]
modtyp<-mod$cov.model

if(modtyp=="exponential"){
modtype<-"Exp"
}else if (modtyp=="spherical"){
modtype<-"Sph"
}else{
stop("Model not supported")
}


spinc<-(spmax-spmin)/100

spacings<-seq(spmin,spmax,spinc)
nsp<-length(spacings)
krigvar<-vector("numeric",nsp)

for (i in 1:nsp){
space<-spacings[i]
krigvar[i]<-okgridvar(space,modtype,c0,c1,a1)
}


op<-cbind(spacings,krigvar)
colnames(op)<-c("Grid.spacing","Kriging.variance")



# Corresponding standard error is SE

SE<-L/1.96

# Corresponding kriging variance is Vark

Vark<-SE^2

TKV<-paste("Target kriging variance =",signif(Vark,5))

plot(spacings,krigvar,type="l",col="red",lwd=2
,xlab="Grid spacing",ylab="Kriging variance",ylim=c(0,max(krigvar)))

mtext(TKV,side=3,adj=0,line=2.5)

if(Vark>krigvar[nsp]){
MSG<-"Confidence interval is very wide, use regional mean to predict"
}else if(Vark<krigvar[1]){
MSG<-"Confidence interval is too fine to attain, adjust or change support"
}else{
sp.index<-which((abs(krigvar-Vark))==(min(abs(krigvar-Vark))))
SPace<-spacings[sp.index]
MSG<-paste("Target achieved with a spacing of",SPace,"units")
#lines(c(SPace,SPace),c(-1,krigvar[sp.index]),lty=5,col="blue")
arrows(SPace, krigvar[sp.index], SPace, 0, length = 0.25, angle = 15,
       code = 2, col ="blue", lty =5)
       
lines(c(-1,SPace),c(krigvar[sp.index],krigvar[sp.index]),lty=5,col="blue")
}
mtext(MSG,side=3,adj=0,line=1)

if(wrout){return(op)}
}
###############################################################
###############################################################
#
#  Functions below not yet documented
#
###############################################################
###############################################################


sv<-function(lag,modtype,c0,c1,a1){

hovera<-lag/a1

#modtype<-"Sph" or "Exp" 

if(modtype=="Exp"){semiv<-c0+c1*(1-exp(-hovera))
} else { 
if(lag>a1){semiv<-c0+c1}
if(lag<=a1){sph<-1.5*hovera-0.5*hovera^3
semiv<-c0+c1*sph}
}
return(semiv)
}

###############################################################
###############################################################

dis<-function(i,j,xg){

dx<-xg[i,1]-xg[j,1]
dy<-xg[i,2]-xg[j,2]
return(sqrt((dx*dx)+(dy*dy)))
}

ddis<-function(i,xg){

dx<-xg[i,1]
dy<-xg[i,2]
return(sqrt((dx*dx)+(dy*dy)))
}

ocskew<-function(x){

percs<-quantile(x,c(1/8,1/2,7/8))
o7<-percs[3]
o4<-percs[2]
o1<-percs[1]

os<-((o7-o4)-(o4-o1))/(o7-o1)
return(as.numeric(os))
}


#####################################################################
na.drop<-function(xin){
noNA<-as.numeric(length(which(is.na(xin)==T)))
if(noNA>0){
x<-as.numeric(na.omit(xin))
print(paste(noNA," missing value(s) removed"),quote=F)
}else{
x<-xin
}
return(x)
}

############################################################################################
#
#  Soil physics:  methods to plot WRC data, fit and plot Van Genuchten models and to 
#   interpret the parameters

############################################################################################
############################################################################################
############################################################################################

VanGenuchten.fit.single<- function(data.df,init.vals,roundoff=4){
  
  theta<-data.df$theta
  h<-data.df$h

  swcc<-function(h,thr,ths,alp,nscal)thr+(ths-thr)/(1+(alp*h)^nscal)^(1-(1/nscal))
  
  nlmvwc<-nls(theta~swcc(h,thr,ths,alp,nscal), data=data.df, start=c(thr=init.vals[1],
  ths=init.vals[2],alp=init.vals[3],nscal=init.vals[4]))
  op<-list("Coefficient.estimates"=as.matrix(round(coef(nlmvwc),roundoff)))
  return(op)
  
}



VanGenuchten.fit.compare<-function(data.df,init.vals,g.name,par.var.null,
par.var.full,cov.name){

#Function to fit VanGenuchten WRC model to data where one or more
#treatments, or other groups, can be compared with respect to one or
#more of the four parameters.  Two models are fitted, a null model with
#some number of parameters common to all groups, and an alternative model
#in which a larger subset of the parameters differ between the groups.

#The coefficients for each group in the alternative model (i.e. with most 
#parameters differing between the groups) are exported as are the 
#log-likelihood ratio for the comparison between the alternative and the null
#models.

#data.df data frame with tensions (kPa) in variable h, VWC (proportion) in
#  variable theta, a factor (g.name) which groups all observations from the
#  same soil sample (or the mean from a single plot), and a factor (cov.name)
#  which specifies the groups to be compares (e.g. treatments)
#
#init.vals a vector with starting point for parameters, in order "thr","ths","alp","nscal"
#par.var.null a vector coding which parameters (same order as init.vals) are constant (0)
# and which vary between the levels of the covariate in the null model.
#par.var.full  a vector coding which parameters (same order as init.vals) are constant (0)
# and which vary between the levels of the covariate in the alternative model.

#g.name  factor which groups observations from a single specimen (or plot average)
#cov.name factor specifying the treatments or other groups to be compared
#


library(saemix)

g.col<-which(names(data.df)==g.name) #which column of data frame contains groupings
ng<-nlevels(data.df[,g.col])

c.col<-which(names(data.df)==cov.name) #which column of data frame contains covariate
Ng<-nlevels(data.df[,c.col])  #nlevels of covariate


saemix.options<-saemixControl(displayProgress=F,print=F,save=F,save.graphs=F,print.is=F) 


wrc.data<-saemixData(data.df,name.group=g.name,
name.predictors="h",name.response="theta",name.covariates=cov.name,verbose=F)

vanGenuchten_null<-saemixModel(model=vanG,
description="Van Genuchten water release curve",
psi0=matrix(init.vals,ncol=4,byrow=T,dimnames=
list(NULL,c("thr","ths","alp","nscal"))),name.response="theta",
covariate.model=matrix(par.var.null,ncol=4,byrow=T),
transform.par=c(3,3,1,1),verbose=F)

pdf(file = NULL)
wrc_null.fit<-saemix(vanGenuchten_null,wrc.data,control=saemix.options)
dev.off()

vanGenuchten_groups<-saemixModel(model=vanG,
description="Van Genuchten water release curve",
psi0=matrix(c(0.07,0.2,0.1,1.2),ncol=4,byrow=T,dimnames=
list(NULL,c("thr","ths","alp","nscal"))),name.response="theta",
covariate.model=matrix(par.var.full,ncol=4,byrow=T),
transform.par=c(3,3,1,1),verbose=F)

pdf(file = NULL)
wrc_groups.fit<-saemix(vanGenuchten_groups,wrc.data,control=saemix.options)
dev.off()

ldf<-sum(par.var.full)-sum(par.var.null)

coeffs<-matrix(nrow=4,ncol=Ng)
rownames(coeffs)<-colnames((coef(wrc_groups.fit))$population$psi)
colnames(coeffs)<-levels(data.df[,which(names(data.df)==cov.name)])


for (i in 1:Ng){ #over levels of factor
index.factor.level<-min(which(data.df[,c.col]==(levels(data.df[,c.col]))[i]))
rep.level<-as.numeric(data.df[index.factor.level,g.col])
coeffs[,i]<-((coef(wrc_groups.fit))$population$psi)[rep.level,]
}


L<-2*(wrc_groups.fit@results@ll.is-wrc_null.fit@results@ll.is)

pval<-1-pchisq(L,ldf)

inference<-list("log.likelihood.ratio"=L,"df"=ldf,"P.value"=pval)
COMP<-cbind(par.var.null,par.var.full)
rownames(COMP)<-c("thr","ths","alp","nscal")
colnames(COMP)<-c("Group.dependent.parameters.null","Group.dependent.parameters.full")
op<-list("Coefficient.estimates"=coeffs,"Comparison"=COMP,"Inference"=inference)

return(op)
}

############################################################################################
VanGenuchten.fit.group<-function(data.df,init.vals,g.name){


#Function to fit VanGenuchten WRC model to a replicated set of observations
# from a single group or treatment.

#The coefficients for each group in the alternative model (i.e. with most 
#parameters differing between the groups) are exported as are the 
#log-likelihood ratio for the comparison between the alternative and the null
#models.

#data.df data frame with tensions (kPa) in variable h, VWC (proportion) in
#  variable theta, a factor (g.name) which groups all observations from the
#  same soil sample (or the mean from a single plot).

#init.vals a vector with starting point for parameters, in order "thr","ths","alp","nscal"
#par.var.null a vector coding which parameters (same order as init.vals) are constant (0)
# and which vary between the levels of the covariate in the null model.
#par.var.full  a vector coding which parameters (same order as init.vals) are constant (0)
# and which vary between the levels of the covariate in the null model.

#g.name  factor which groups observations from a single specimen (or plot average)
#cov.name factor specifying the treatments or other groups to be compared
#


library(saemix)

g.col<-which(names(data.df)==g.name) #which column of data frame contains groupings
ng<-nlevels(data.df[,g.col])

saemix.options<-saemixControl(displayProgress=F,print=F,save=F,save.graphs=F,print.is=F) 


wrc.data<-saemixData(data.df,name.group=g.name,
name.predictors="h",name.response="theta",verbose=F)

vanGenuchten<-saemixModel(model=vanG,
description="Van Genuchten water release curve",
psi0=matrix(init.vals,ncol=4,byrow=T,dimnames=
list(NULL,c("thr","ths","alp","nscal"))),name.response="theta",
transform.par=c(3,3,1,1),verbose=F)

pdf(file = NULL)
wrc.fit<-saemix(vanGenuchten,wrc.data,control=saemix.options)
dev.off()


coeffs<-matrix(nrow=4,ncol=1)
sterr<-matrix(nrow=4,ncol=1)
rownames(coeffs)<-wrc.fit@results@name.fixed
rownames(sterr)<-wrc.fit@results@name.fixed
colnames(coeffs)<-c("Estimate")
colnames(sterr)<-c("Standard error")
coeffs[,1]<-wrc.fit@results@fixed.effects
sterr[,1]<-wrc.fit@results@se.fixed

op<-list("Coefficient.estimates"=coeffs,"Standard.errors"=sterr)

return(op)
}


############################################################################################

vanG<-function(ps1,id,xidep){
h<-xidep[,1]
thr<-ps1[id,1]
ths<-ps1[id,2]
alp<-ps1[id,3]
nscal<-ps1[id,4]
mscal<-mscal<-1-1/nscal
den<-(1+((abs(alp*h))^nscal))^mscal
return(thr+((ths-thr)/den))
}



vanGdraw<-function(psi,h){
thr<-psi[1]
ths<-psi[2]
alp<-psi[3]
nscal<-psi[4]
mscal<-1-(1/nscal)
den<-(1+((abs(alp*h))^nscal))^mscal
vwc<-thr+((ths-thr)/den)
return(as.numeric(vwc))
}


plot.wrc.data<-function(data.df,coeffs,hvar="h",thetavar="theta",
xlab="Tension /kPa",maxth=-1,groups="none",main=""){
if(missing(coeffs)){models=FALSE}else{models=TRUE}

#Function to plot WRC data from dataframe data.df
#hvar optional name for tension variable in data.df, default "h"
#thetavar optional name for vwc in data.df, default "theta"
#xlab optional label for tension axis, default "Tension /kPa",
#maxth optional maximum vwc for plot 
#groups optional name for groups to be distinguished on plot
#main  optional main title for plot

library(RColorBrewer)


mypalette<-brewer.pal(9,"Set1")
n<-nrow(data.df)

hcol=which(names(data.df)==hvar)
thcol=which(names(data.df)==thetavar)
if(groups!="none"){
gcol=which(names(data.df)==groups)
gnames<-as.character(levels(data.df[,gcol]))
ngr<-length(gnames)
cols<-mypalette[as.numeric((data.df[,gcol]))]
}else{
cols<-rep("black",n)
}
if(maxth<0){
ylim=c(0,(max(data.df[,thcol])*1.01))}else{
ylim=c(0,maxth)
}

options("scipen"=100, "digits"=4) 
plot(data.df[,hcol],data.df[,thcol],ylim=ylim,xlab=xlab,ylab=expression(paste(theta)),
pch=16,col=cols,log="x",main=main)
if(groups!="none"){
legend("bottomleft", legend = gnames,
pch = 16, col = mypalette)
}

if(models==TRUE){
H<-c(seq(0.05,1,0.001),seq(1,1500,0.1))
if(groups=="none"){
  thdr<-vanGdraw(coeffs[,1],H)
  lines(H,thdr,lwd=2)
  }else{
for (i in 1:ngr){
  thdr<-vanGdraw(coeffs[,i],H)
  lines(H,thdr,lwd=2,col=mypalette[i])
}
}
}

}


SQ.indices<-function(coeffs,bulk_density,t_m=4.9,t_FC=9.8, t_PWP=1471){

# coeffs: coefficients for m soils in wrc coefficient format
# i.e. a 4 x m matrix in which the rows correspond respectively
# to thr, ths, alp, nscal
#
# bulk_density an optional argument.  A m-vector of bulk density values
# (g cm^-3).  If these are not provided then the value is inferred from
# ths, assuming a particle density of 2.65 g cm^-3 (this may underestimate
# particle density for very ferruginous soils, and overestimate it for soils
# with a large organic content.  (Landon, J.R. (ed), 1984.  Booker Tropical
# Soil Manual (1st Edn) Longman, Harlow.  page 97, section 6.5).  
# Required to compute Dexter's S value.
#
# t_m tension (kPa) at which VWC ~ matrix porosity.  Default 4.9 kPa following
#  Reynolds et al. (2007) (page 318, max. equiv. pore diameter of 0.06 mm.
# t_FC tension (kPa) for field capacity.  Default 9.8 following Reynolds
# t_PWP tension (kPa) for permanent wilting point.  Default 1471 following Reynolds


ncl<-ncol(coeffs)
if (missing(bulk_density)){
pd<-2.65
bulk_density<-pd*(1-coeffs[2,])
}

noutp<-matrix(nrow=ncl,ncol=6)
rownames(noutp)<-colnames(coeffs)
colnames(noutp)<-c("Dexter.S","Total_Porosity","Macropores","Relative_Water_Capacity",
            "Plant_Available_Water_Capacity","Air_Capacity")

interp<-matrix(" ",nrow=ncl,ncol=6)
rownames(interp)<-colnames(coeffs)
colnames(interp)<-c("Dexter.S","Total_Porosity","Macropores","Relative_Water_Capacity",
            "Plant_Available_Water_Capacity","Air_Capacity")


for(i in 1:ncl){


thr<-coeffs[1,i]
ths<-coeffs[2,i]
alp<-coeffs[3,i]
nscal<-coeffs[4,i]
bd<-bulk_density[i]

# i.  Dexter S

thr_g<-thr/bd #convert volumetric WC to gravimetric (see Dexter (2004) 
ths_g<-ths/bd # section 6 paragraph 2

noutp[i,1]<-round((nscal)*(ths_g-thr_g)*((((2*nscal)-1)/(nscal-1))^((1/nscal)-2)),3)

Inter<-"Poor microstructural quality"
if(noutp[i,1]>0.035){Inter<-"Good microstructural quality"}
if(noutp[i,1]<=0.02){Inter<-"Very poor microstructural quality"}

interp[i,1]<-Inter
# ii.  Total porosity

noutp[i,2]<-ths

# iii. Macroporosity following Reynolds et al, 2007 

thm<-vanGdraw(coeffs[,1],t_m) #matrix porosity
noutp[i,3]<-round((ths-thm),2)

if(noutp[i,3]<=0.04){Inter<-"Macroporosity degraded"
}else{Inter<-"Undegraded (medium to fine textured)"}

interp[i,3]<-Inter

# iv. Relative water capacity Reynolds et al, 2007 

thFC<-vanGdraw(coeffs[,1],t_FC)
noutp[i,4]<-round((thFC/ths),1)

Inter<-"RWC in optimal range for microbial activity"
if(noutp[i,4]<=0.6){Inter<-"RWC suboptimal for microbial activity"}
if(noutp[i,4]>0.7){Inter<-"RWC exceeds range for optimal microbial activity"}


interp[i,4]<-Inter

# v. Plant available water capacity 

thPWP<-vanGdraw(coeffs[,1],t_PWP)
noutp[i,5]<-round(thFC-thPWP,2)
if(noutp[i,5]>0.2){Inter<-"PAW ideal"}
if(noutp[i,5]<=0.2){Inter<-"PAW good"}
if(noutp[i,5]<=0.15){Inter<-"PAW limited"}
if(noutp[i,5]<=0.10){Inter<--"PAW poor"}


interp[i,5]<-Inter



#vi.  Air capacity

noutp[i,6]<-round(ths-thFC,2)

Inter<-"AC adequate, but marginal in fine-textured soil"
if(noutp[i,6]>0.15){Inter<-"AC good for all soils"}
if(noutp[i,6]<=0.1){Inter<-"Aeration deficit likely in rooting zone"}

interp[i,6]<-Inter

} #end of loop over classes

interp<-interp[,-2]

return(list("Indices"=noutp,"Interpretation"=interp))
}











############################################################################################
#
#  Methods for censored (log)normal variables
#

mean_censor<-function(y,cen,log.t){
if(missing(log.t)){log.t=F}

mean.guess<-mean(y)
sd.guess<-sd(y)

ncen<-length(which(is.na(censor(y,cen))))
oo<-optim(c(mean.guess,sd.guess),nllcen,cen=cen,y=y)
oo2<-optim(oo$par,nllcen,cen=cen,y=y,hessian=T)

se<-sqrt((solve(oo2$hessian))[1,1])

mean.est<-oo2$par[1]
sd.est<-oo2$par[2]

# approximate df by n-1

nobs<-length(y)
tval<-qt(0.975,(nobs-1))
clu<-oo2$par[1]+tval*se
cll<-oo2$par[1]-tval*se

mean.naive<-mean(censor(y,cen),na.rm=T)

op1<-matrix(c((-1*oo2$value),ncen,mean.est,sd.est,cll,clu,mean.naive),1,7)
colnames(op1)<-c("log_likelihood","Number_censored","Mean","SD","Lower_95%","Upper_95%","Mean_>_DL_only")



if(log.t==T){

median.unbiased<-exp(mean.est)
cll.bt<-exp(cll)
clu.bt<-exp(clu)


op2<-matrix(c(median.unbiased,cll.bt,clu.bt),1,3)
colnames(op2)<-c("Back_median","Back_Lower_95%","Back_Upper_95%")
op1<-cbind(op1,op2)
}

return(op1)
}

#
censor<-function(x,cen){
censored<-which(x<cen)
x[censored]<-NA
return(x)
}

nllcen<-function(theta,cen,y){
#
#
#
mu<-theta[1]
sig<-theta[2]

y<-censor(y,cen)
n_c<-length(which(is.na(y)))
Phi_c<-log(pnorm(cen,mu,sig))
z<-y[-which(is.na(y))]

dens<-dnorm(z,mu,sig,log=T)

return(-n_c*Phi_c-sum(dens))
}



