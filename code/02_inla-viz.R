
library(ggplot2)
library(tmap)
# From https://rdrr.io/github/gfalbery/ggregplot/
library(ggregplot)
#install.packages("MCMCglmm")
library(stringr)
library(magrittr)
library(raster)  #raster data
library(gridExtra)
library(RColorBrewer)
library(fields) # colour function for levelplot
library(lattice) # Mapping residual + Malawi map
options(scipen=999)
#ggplot(plasma_se, aes(Longitude, Latitude )) + 
#  geom_jitter(aes(colour = factor(survey_cluster1))) + coord_fixed() + 
#  labs(colour = "survey_cluster1")

modelNames <-  c("buffer10",
           "buffer15",
           "buffer20",
           "buffer25",
           "buffer30",
           "buffer40",
           "buffer50",
           "buffer60",
           "EA group",
           "district", 
           "region")

gps <- plasma_se %>% dplyr::select(survey_cluster1, Longitude, Latitude) %>% 
  distinct() %>% arrange(survey_cluster1)

# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this



# Checks
# https://julianfaraway.github.io/brinla/examples/chicago.html
# Checking CPO observed response based on model fit (small no. = unexpected?)
n = nrow(plasma_se)
plot(1:n,models[[11]]$cpo$cpo, ylab="CPO",type="n")
text(1:n,models[[11]]$cpo$cpo, 1:n)

# PIT new responses less observed (uniformedly distributed)
pit <- models[[11]]$cpo$pit
uniquant <- (1:n)/(n+1)
plot(uniquant, sort(pit), xlab="uniform quantiles", ylab="Sorted PIT values")
abline(0,1)

# Issues with low values
plot(logit(uniquant), logit(sort(pit)), xlab="uniform quantiles", ylab="Sorted PIT values", main="Logit scale")
abline(0,1)
which(logit(pit) <(-4))

View(plasma_se[which(logit(pit) <(-4)),])

plot(log(plasma_se$crp), pit, xlab="CRP", ylab="PIT")
plot(log(plasma_se$Se_mean), pit, xlab="Mazie Se", ylab="PIT")
#text(log(plasma_se$crp), pit, 1:n)

#DIC
models[[11]]$dic$dic


# Visualisation ------

# Quick summary plots
plot(models[[11]])

INLADICFig(models[c(1:10)]) + theme_bw() + 
 # scale_x_discrete(labels= modelNames[1:10]) +
  theme(legend.position = "none", 
  strip.text = element_text(size = 12),
  axis.text.y = element_text(size = 12), 
  axis.text.x = element_text(size = 10))

Efxplot(models[c(1:10)]) + theme_bw() +
  labs(y = "") +
 # scale_colour_discrete(labels = modelNames[1:10])+
  theme(
        strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10))


Maxrange = 1000

INLARange(list(models[[1]]), MaxRange = Maxrange)

INLARange(ModelList = models, MaxRange= 1, MeshList = list(mesh))

# Results ----

## Getting fixed effect -----

fix.effect <-  round(models[[2]]$summary.fixed, 4)
write.csv(round(models[[9]]$summary.fixed, 4), here::here("output", "fixed.effect-cluster_v1.0.0.csv"))

fix.effect <-  NA

for(i in 1:length(models)){
  
  fix.effect <- cbind(fix.effect, round(models[[i]]$summary.fixed, 4))
  
}

 write.csv(fix.effect, here::here("output", "fixed.effect_v2.0.1.csv"))



# Visualising the mean and CI of  the 11 models

#Storing
M <- NA
L <- NA
U <- NA

for(i in 1:length(models)){

# Mean
m <- round(models[[i]]$summary.fixed[2,1], 4)
# .25
lo <- round(models[[i]]$summary.fixed[2,3], 4)
#.097
up <- round(models[[i]]$summary.fixed[2,5], 4)

#Storing
M[i] <- m
L[i] <- lo
U[i] <- up

}

dat <- cbind(modelNames,M,L, U)

ggplot(dat[c(1:10),], aes( modelNames, as.numeric(M))) +
  geom_point() +
  geom_linerange(ymin = L[1:10], ymax = U[1:10]) +
  scale_y_continuous(limits = c(-0.20, 0.6)) +
  theme_bw() +
  labs(y = "", x = "") +
  theme(
    strip.text = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 10))

## Visualising model parameters for all models
# Krainski and Rue,  2017.
# https://inla.r-inla-download.org/r-inla.org/doc/vignettes/SPDEhowto.pdf

# Testing...
pmd.s2e <- lapply(models, function(m) inla.tmarginal(function(x) sqrt(1/x), ## inverse and square root
                          m$marginals.hyperpar$'Precision for the Gaussian observations'))

# Error term (noise or nugget effect)
plot(pmd.s2e[[1]], type='l', ylab='Density', xlab=expression(sigma[e]))
#abline(v=sqrt(s2e), col=2) ## add the 'true' value

# spatial RF (sigma)
plot(models[[1]]$marginals.hy[[3]], type='l', xlab=expression(sigma[u]), ylab='Density')
#abline(v=sqrt(s2u), col=2) ## add the 'true' value

# (spatial) range
plot(models[[1]]$marginals.hy[[2]], type='l', xlab='range nominal', ylab='Density')
#abline(v=r, col=2) ## add the 'true' value

# Viz
rcols <- rainbow(10) ##c(rgb(4:1/4,0:3/5,0), c(rgb(0,0:3/5,4:1/4))) 
par(mfrow=c(1,3), mar=c(2.5,2.5,1,.5), mgp=c(1.5,.5,0), las=1)

plot(pmd.s2e[[4]], type='l', ylab='Density', xlab=expression(sigma[e]))
for (k in 1:length(models))
lines(pmd.s2e[[k]], col=rcols[k], lwd=2, ylab='Density', xlab=expression(sigma[e]))

plot(models[[4]]$marginals.hy[[3]], type='l', xlab=expression(sigma[u]), ylab='Density')
for (k in 1:length(models))
lines(models[[k]]$marginals.hy[[3]], col=rcols[k], lwd=2, xlab=expression(sigma[u]), ylab='Density')

plot(models[[4]]$marginals.hy[[2]], type='l', xlab='range nominal', ylab='Density')
for (k in 1:length(models))
lines(models[[k]]$marginals.hy[[2]], col=rcols[k], lwd=2, xlab='range nominal', ylab='Density')

legend('topright', c(paste('buffer',1:8, sep=''), 'cluster', 'district'), 
       lty=c(rep(1,10), 2, 3), lwd=rep(2, 10), col=c(rcols), bty='n')




## Visualising model parameters for all models
# (Krainski et al., p. 37)

s2.marg <- lapply(models, function(m) inla.tmarginal(function(x) 1/x, m$marginals.hy[[1]]))

#Extracting the SPDE results for all the models
spde.est <- lapply(models, function(m) inla.spde2.result(m, name = "spatial.field",
                              spde = spde, do.transf = TRUE))

# looping over all model results
rcols <- rainbow(10) ##c(rgb(4:1/4,0:3/5,0), c(rgb(0,0:3/5,4:1/4))) 
par(mfrow=c(2,3), mar=c(2.5,2.5,1,.5), mgp=c(1.5,.5,0), las=1)
xrange <- range(sapply(models, function(x) range(x$marginals.fix[[1]][,1]))) 
yrange <- range(sapply(models, function(x) range(x$marginals.fix[[1]][,2]))) 
plot(models[[1]]$marginals.fix[[1]], type='l', xlim=xrange, ylim=yrange, 
    # xlab=expression(beta[0]), ylab='Density')
     xlab=expression(alpha[c]), ylab='Density')

for (k in 1:length(models))
  lines(models[[k]]$marginals.fix[[1]], col=rcols[k], lwd=2)
xrange <- range(sapply(s2.marg, function(x) range(x[,1])))
yrange <- range(sapply(s2.marg, function(x) range(x[,2])))
  plot.default(s2.marg[[k]], type='l', xlim=xrange, 
                ylim=yrange, xlab=expression(sigma[e]^2), ylab='Density')

for (k in 1:length(models))
  lines(s2.marg[[k]], col=rcols[k], lwd=2)
  xrange <- range(sapply(spde.est, function(r) range(r$marginals.variance.nominal[[1]][,1])))
  yrange <- range(sapply(spde.est, function(r) range(r$marginals.variance.nominal[[1]][,2]))) 
plot(spde.est[[1]]$marginals.variance.nominal[[1]], type='l', xlim=xrange, ylim=yrange, 
    # xlab=expression(sigma[x]^2), ylab='Density')
     xlab=expression(sigma[omega]^2), ylab='Density')

for (k in 1:length(models))
  lines(spde.est[[k]]$marginals.variance.nominal[[1]], col=rcols[k], lwd=2)
xrange <- range(sapply(spde.est, function(r) range(r$marginals.kappa[[1]][,1]))) 
yrange <- range(sapply(spde.est, function(r) range(r$marginals.kappa[[1]][,2])))
plot(spde.est[[1]]$marginals.kappa[[1]], type='l', xlim=xrange, ylim=yrange, 
     xlab=expression(kappa), ylab='Density') 

for (k in 1:length(models))
  lines(spde.est[[k]]$marginals.kappa[[1]], col=rcols[k], lwd=2)
xrange <- range(sapply(spde.est, function(r) range(r$marginals.range.nominal[[1]][,1])))
yrange <- range(sapply(spde.est, function(r) range(r$marginals.range.nominal[[1]][,2])))
plot(spde.est[[1]]$marginals.range.nominal[[1]], type='l', xlim=xrange,
     ylim=yrange, xlab='nominal range', ylab='Density') 

for (k in 1:length(models))
  lines(spde.est[[k]]$marginals.range.nominal[[1]], col=rcols[k], lwd=2)
xrange <- range(sapply(spde.est, function(r) range(r$marginals.tau[[1]][,1])))
yrange <- range(sapply(spde.est, function(r) range(r$marginals.tau[[1]][,2])))
plot(spde.est[[1]]$marginals.tau[[1]], type='l', xlim=xrange, ylim=yrange, xlab=expression(tau), ylab='Density') 

for (k in 1:length(models)) 
  lines(spde.est[[k]]$marginals.tau[[1]], col=rcols[k], lwd=2)
legend('topright', c(paste('buffer',1:8, sep=''), 'cluster', 'district'), 
       lty=c(rep(1,10), 2, 3), lwd=rep(2, 10), col=c(rcols), bty='n')



# Plotting alpha (https://becarioprecario.bitbucket.io/spde-gitbook/ch-INLA.html)
plot(models[[9]]$marginals.fixed[[1]], type = "l", 
     xlab = expression(alpha), ylab = "density")

# Compute posterior marginal of variance
post.var <- inla.tmarginal(function(x) exp(-x), 
                           models[[9]]$internal.marginals.hyperpar[[1]])
# Compute summary statistics
inla.zmarginal(post.var)

i = 11
# Compute posterior marginal of variance
post.var <- inla.tmarginal(function(x) exp(-x), 
                           models[[i]]$internal.marginals.hyperpar[[1]])
# Compute summary statistics
inla.zmarginal(post.var)

# Spatial Results ----

round(models[[9]]$summary.fix, 4)

w.pm <- models[[9]]$summary.random$spatial.field$mean
w.sd <- models[[9]]$summary.random$spatial.field$s


# Checking underlying spatial process 
#Compute statistics in terms or range and variance
#spde.est <- inla.spde2.result(inla = models[[9]], name = "spatial.field",
 #                             spde = spde, do.transf = TRUE)


#Kappa
inla.zmarginal(spde.est[[9]]$marginals.kappa[[1]])
inla.zmarginal(spde.est[[10]]$marginals.kappa[[1]])

Kappa <- inla.emarginal(function(x) x, 
                        spde.est[[9]]$marginals.kappa[[1]] )

inla.zmarginal(sapply(spde.est, function(x)
  inla.zmarginal(x$marginals.kappa[[1]])))



## Residual spatial variation ----

## Map: viz -----

# Building the projection matrix (mesh) of the spatial field 

rang <-  apply(mesh$loc[, c(1,2)], 2 , range)
proj <- inla.mesh.projector(mesh, xlim = rang[ , 1],
                            ylim = rang[ , 2],
                            dims = c(200, 200)) 

# Storing mean maps 
mean <- list()

# Storing sd maps
sd <- list()

# Base map
#base <- ggplot() + geom_sf(data = dist_bnd, colour = "grey", fill = NA)

# Looping over all model resutls
for(i in 1:length(models[c(1:10)])){
  
# Extracting the spatial field values (mean and sd of residual spatial field)
  
mean_s <- inla.mesh.project(proj, 
                            models[[i]]$summary.random$spatial.field$mean)
sd_s <- inla.mesh.project(proj, 
                          models[[i]]$summary.random$spatial.field$sd)


# Storing the data together as dataframe
df <- expand.grid(x = proj$x, y = proj$y)
df$mean_s <- as.vector(mean_s)
df$sd_s <- as.vector(sd_s)
names(df) <- c("x", "y", "Mean", "SD") # Change names

# Mapping the spatial results
mean[[i]] <-  ggplot(df, aes(x = x, y = y, fill = Mean)) +
  geom_raster() +
  scale_fill_viridis_b(na.value = "transparent") +
  coord_fixed(ratio = 1) + 
#  geom_sf(data = dist_bnd, colour = "grey", fill = NA) +
 # labs(title = gsub("_v2.0.0.RDS", "", file[i])) +
  labs(title = modelNames[i]) +
   theme_bw()


sd[[i]] <-  ggplot(df, aes(x = x, y = y, fill = SD)) +
  geom_raster() +
  scale_fill_viridis_b(na.value = "transparent") +
  coord_fixed(ratio = 1) + theme_bw()

}


# Saving into one pdf

pdf("visuals/spatial.pdf", onefile = TRUE)

for(i in 1:length(mean)){
  
  grid.arrange(mean[[i]], sd[[i]])
}

dev.off()



## Map 2: Spatial residual simple ----

pdf("visuals/spatial-residual.pdf", onefile = TRUE)
par(mfrow = c(1, 2), mar = c(5, 3, 4, 2))

for(i in 1:length(models[c(1:10)])){
  
  # Extracting the spatial field values (mean and sd of residual spatial field)
  
  mean_s <- inla.mesh.project(proj, 
                              models[[i]]$summary.random$spatial.field$mean)
  sd_s <- inla.mesh.project(proj, 
                            models[[i]]$summary.random$spatial.field$sd)
  
  
  # Storing the data together as dataframe
  df <- expand.grid(x = proj$x, y = proj$y)
  df$mean_s <- as.vector(mean_s)
  df$sd_s <- as.vector(sd_s)
  
  # Converting into raster
  dfr <- rasterFromXYZ(df)  #Convert first two
  names(dfr) <- c("Mean", "SD") # Change names
  
  plot(dfr, "Mean", xlim =c(30, 38))
  lines(dist_bnd, col="darkgrey", lwd = 1.5)
 # points(coord, col = "red", pch = 1)
  plot(dfr, "SD", xlim =c(30, 38))
  lines(dist_bnd, col="darkgrey", lwd = 1.5)
 #  points(coord, col = "red", pch = 1)
  
}  

dev.off()





## Adding Malawi map into the residual map ----

# Converting from sf to spatial polygon
mwi <-  as(dist_bnd, 'Spatial')

# Storing mean maps 
mean <- list()

# Storing sd maps
sd <- list()

# For storing into pdf

pdf("visuals/spatial-field_and_map.pdf", onefile = TRUE)

# Looping over all model resutls
for(i in 1:length(models[c(1:10)])){
  
  # Extracting the spatial field values (mean and sd of residual spatial field)
  
  mean_s <- inla.mesh.project(proj, 
                              models[[i]]$summary.random$spatial.field$mean)
  sd_s <- inla.mesh.project(proj, 
                            models[[i]]$summary.random$spatial.field$sd)
  
  # Storing the data together as dataframe
  df <- expand.grid(x = proj$x, y = proj$y)
  df$mean_s <- as.vector(mean_s)
  df$sd_s <- as.vector(sd_s)
  
### for grey colour scale: col.regions=gray.colors(16,start=1,end=0)
  #https://github.com/GodinA/cjfas-bycatch-INLA-SPDE/blob/master/Rcodes/Suppl_Material.R
  mean[[i]] <- levelplot(mean_s ~ x*y, data=df,xlab='', ylab='',# xlim = c(30, 38),
          #col.regions=tim.colors(100),
           main=modelNames[i], sub = "Mean", 
          col.regions=gray.colors(16,start=1,end=0),  scale=list(draw=FALSE),
          par.strip.text=list(cex=2),strip = strip.custom(bg="white")) + 
  latticeExtra::layer(sp::sp.polygons(mwi, col = 1, alpha =0.4, fill="transparent"))


  sd[[i]] <- levelplot(sd_s ~ x*y, data=df,xlab='', ylab='',
                      # col.regions=tim.colors(100),
                       sub="SD", 
                       col.regions=gray.colors(16,start=1,end=0), scale=list(draw=FALSE),
                       par.strip.text=list(cex=2),strip = strip.custom(bg="white")) + 
    latticeExtra::layer(sp::sp.polygons(mwi, col = 1, alpha =0.4, fill="transparent"))
  
grid.arrange(mean[[i]], sd[[i]])

}

dev.off()


#(31.34366,37.30330) x (-18.741212, -7.858583)

new_mwi <- mwi
st_bbox(new_mwi) 

# st_bbox(31.34366,-18.741212, 37.30330, -7.858583)

# Variance --------
inla.zmarginal(spde.est$marginals.variance.nominal[[1]])

Sigma_u <- inla.emarginal(function(x) x, 
                          spde.est$marginals.variance.nominal[[1]])

inla.zmarginal(sapply(spde.est, function(x)
  inla.zmarginal(x$marginals.variance.nominal[[1]])))



#Range
r <- inla.zmarginal(spde.est$marginals.range.nominal[[1]])


# This is perhaps a nicer graph to make and present.
# Show correlation structure
# First we obtain the locations of each point of the mesh.
LocMesh <- mesh$loc[,1:2]

# And then we calculate the distance between each vertex.
D <- as.matrix(dist(LocMesh))

# Using the estimated parameters from the model (see above)
# we can calculate the imposed Matern correlation values.
d.vec <- seq(0, max(D), length = 732)      
Cor.M <- (Kappa * d.vec) * besselK(Kappa * d.vec, 1) 
Cor.M[1] <- 1

# Which we plot here:
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = d.vec, 
     y = Cor.M, 
     pch = 16, 
     type = "l", 
     cex.lab = 1.5,
     xlab = "Distance (km)", 
     ylab = "Correlation",
     xlim = c(0, 20))




############################
# 13.14 Model validation
# This is the model we will focus on.
# There is no need to run it again, but we
# do.

N <- nrow(plasma_se)

# Get fitted values and residuals
Fit9 <- models[[9]]$summary.fitted.values[1:N,"mean"]
E9     <- plasma_se$Plasma_Se - exp(Fit9)


#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit9, y = E9)
abline(h = 0, v = 0)

#Normality
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(E9, breaks = 25)

# Independence due to model misfit
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = plasma_se$dist_to_lake, 
     y = E9)
abline(h = 0)

plot(x=plasma_se$AGE_IN_YEARS, y = E9)
abline(h = 0)

boxplot(E9 ~ wealth_quintile, data = plasma_se)
abline(h = 0)

##########################################
# Spatial patterns in the residuals?
# Option 1: Plot the residuals vs spatial locations.
#            Look for patterns.
# Option 1:
MyCex <- 3 * abs(E9) / max(E9) + 0.5
Sign <- as.numeric(E9 >=0) + 1
MyPch <- c(1, 16)[Sign]
lattice::xyplot(Latitude ~ Longitude,
       data = plasma_se,
     #  cex = MyCex,
       pch = MyPch,
       col = 1,
       aspect = "iso",
       xlab = list(label = "Longitude", cex = 1.5),
       ylab = list(label = "Latitude", cex = 1.5)
)
# Better as before!!!!

hist(models[[9]]$summary.random$ID$mean)
hist(models[[10]]$summary.random$ID$mean)

cluster.sd <- models[[9]]$summary.random$ID$sd
cluster.mean <- models[[9]]$summary.random$ID$mean


cluster.sd <- cbind(gps, cluster.sd)

cluster <- cbind(gps, cluster.mean)

cluster.sd %>% 
st_as_sf(., coords = c('Longitude', 'Latitude'))  %>% 
  ggplot() + 
  geom_sf(aes(color = cluster.sd), size =2) +
  theme_bw()

cluster %>% 
  st_as_sf(., coords = c('Longitude', 'Latitude'))  %>% 
  ggplot() + 
  geom_sf(aes(color = cluster.mean), size =2) +
  theme_bw()



## Linear predictor -----

i = 9

idx.obs <- inla.stack.index(stack, tag = "est")$data ## index in the stack
order.eta <- order(models[[i]]$summary.fitted.values$mean[idx.obs])
plot(log(plasma_se$Plasma_Se[order.eta]), pch=19, ylab='y')
segments(1:n, models[[i]]$summary.fitted.val$'0.025quant'[idx.obs][order.eta],
         1:n, models[[i]]$summary.fitted.val$'0.975quant'[idx.obs][order.eta])

models[[i]]$marginals.fitted.values[idx.obs]


