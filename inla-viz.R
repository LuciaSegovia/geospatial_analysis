
library(ggplot2)
library(ggregplot)
#install.packages("MCMCglmm")
library(stringr)
library(magrittr)

#ggplot(plasma_se, aes(Longitude, Latitude )) + 
#  geom_jitter(aes(colour = factor(survey_cluster1))) + coord_fixed() + 
#  labs(colour = "survey_cluster1")

gps <- plasma_se %>% select(survey_cluster1, Longitude, Latitude) %>% 
  distinct() %>% arrange(survey_cluster1)


# Visualisation ------

INLADICFig(models)

Efxplot(models)



# Results ----

## Getting fixed effect -----

fix.effect <-  models[[6]]$summary.fixed

for(i in 7:10){
  
  fix.effect <- cbind(fix.effect, models[[i]]$summary.fixed )
  
}

# write.csv(round(fix.effect,2), here::here("output", "fixed.effect_v1.0.0.csv"))

# Plotting alpha (https://becarioprecario.bitbucket.io/spde-gitbook/ch-INLA.html)
plot(m0$marginals.fixed[[1]], type = "l", 
     xlab = expression(alpha), ylab = "density")

# Compute posterior marginal of variance
post.var <- inla.tmarginal(function(x) exp(-x), 
                           models[[9]]$internal.marginals.hyperpar[[1]])
# Compute summary statistics
inla.zmarginal(post.var)



## Spatial Results ----

round(models[[9]]$summary.fix, 4)

w.pm <- models[[9]]$summary.random$spatial.field$mean
w.sd <- models[[9]]$summary.random$spatial.field$s


# Checking underlying spatial process 
#Compute statistics in terms or range and variance
spde.est <- inla.spde2.result(inla = models[[9]], name = "spatial.field",
                              spde = spde, do.transf = TRUE)


#Kappa
inla.zmarginal(spde.est$marginals.kappa[[1]])

Kappa <- inla.emarginal(function(x) x, 
                        spde.est$marginals.kappa[[1]] )
# Variance --------
inla.zmarginal(spde.est$marginals.variance.nominal[[1]])

Sigma_u <- inla.emarginal(function(x) x, 
                          spde.est$marginals.variance.nominal[[1]])

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


# Get fitted values and residuals
Fit9 <- models[[9]]$summary.fitted.values[1:732,"mean"]
E9     <- plasma_se$Plasma_Se - exp(Fit9)


#Homogeneity
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = Fit9, y = E9)
abline(h = 0, v = 0)

#Normality
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
hist(E9, breaks = 25)

#Independence due to model misfit
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
# Option 1: Plot the residuals vs spatial
#           locations. Look for patterns.
# Option 1:
MyCex <- 3 * abs(E9) / max(E9) + 0.5
Sign <- as.numeric(E9 >=0) + 1
MyPch <- c(1, 16)[Sign]
lattice::xyplot(Latitude ~ Longitude,
       data = plasma_se,
       cex = MyCex,
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


