

###################################################################################################
#
# Kriging FUNCTIONS
#
###################################################################################################

############################################################################
#
#  Matern correlation function
#
#

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


############################

wls<-function(theta){
  
  c0<-theta[1]
  c1<-theta[2]
  phi<-theta[3]
  
  
  nh<-length(h)
  
  wss<-0
  
  for (i in 1:nh){
    svm<-c0+c1*(1-matern(h[i],phi,kap))
    wss<-wss+npa[i]*((sv[i]-svm)^2)
  }
  wss<-wss/nh
  return(wss)
}


wlsnugg<-function(theta){
  
  c0<-theta[1]
  
  nh<-length(h)
  
  wss<-0
  
  for (i in 1:nh){
    svm<-c0
    wss<-wss+npa[i]*((sv[i]-svm)^2)
  }
  wss<-wss/nh
  return(wss)
}


