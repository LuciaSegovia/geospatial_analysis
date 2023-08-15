

library("rgdal")
library("sp")
library("spData")
library("INLA")


# Testing INLA package
n = 100; a = 1; b = 1; tau = 100
z = rnorm(n)
eta = a + b*z

scale = exp(rnorm(n))
prec = scale*tau
y = rnorm(n, mean = eta, sd = 1/sqrt(prec))


data = list(y=y, z=z)
formula = y ~ 1+z
result = inla(formula, family = "gaussian", data = data)

summary(result)


# Testing lattice modelling

boston.tr <- readOGR(system.file("shapes/boston_tracts.shp",
  package="spData")[1])

class(boston.tr)
head(boston.tr)

boston.adj <- poly2nb(boston.tr)

head(boston.adj)

W.boston <- nb2mat(boston.adj, style = "B") 

W.boston.rs <- nb2mat(boston.adj, style = "W") 

dim(boston.tr)
names(boston.tr)

plot(boston.tr[,"AGE"])

tm_shape(boston.tr) +
tm_polygons(col = "AGE")


# Testing point data

library("spatstat")
data(clmfires)

#Subset to 2004 to 2007
clmfires0407 <- clmfires[clmfires$marks$date >= "2004-01-01"]

clm.bdy <- do.call(cbind, clmfires0407$window$bdry[[1]])
plot(clm.bdy)

#Define mesh
clm.mesh <- inla.mesh.2d(loc.domain = clm.bdy, max.edge = c(15, 50),
  offset = c(10, 10))

plot(clm.mesh)

clm.mesh$loc
