

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

clmbdy.sp <- SpatialPolygons(list(Polygons(list(Polygon (clm.bdy)),
  ID = "1"))) 

plot(clmbdy.sp,  border ="green", lwd =4, add = TRUE)

#Define mesh
mwi.mesh <- inla.mesh.2d(loc = malawi_bnd)

plot(clm.mesh)

mwibdy.sp <- SpatialPolygons(list(Polygons(list(Polygon (malawi_bnd)),
  ID = "1"))) 

plot(malawi_bnd,  border ="green", lwd =4, add = TRUE)

class(clm.bdy)
class(malawi_bnd)
st_coordinates(malawi_bnd)

# Generating a mesh 
# Example from: (Lindgren and Rue, 2015, p. 10)
m  <-  50
points  <-  matrix(runif(m * 2), m, 2)

mesh  <-  inla.mesh.2d( loc = points, 
cutoff = 0.05,    # (optional) to avoid many small triangles in clustered locations
offset = c(0.1, 0.4),  # (optional) inner and outer extensions around the data locations
max.edge = c(0.05, 0.5))  # maximum allowed triangle edge lengths in the inner domain and in the outer extension


plot(mesh)

bnd  <-  inla.nonconvex.hull(points, convex = 0.12)

mesh  <-  inla.mesh.2d(boundary = bnd, 
cutoff = 0.05, max.edge = c(0.1) )

mesh  <-  inla.mesh.2d(boundary = bnd, 
cutoff = 0.01,      # the smaller the closer the innner triangles
 offset = c(1, 0.5), 
 max.edge = c(0.1, 0.5) )

plot(mesh)

# Creating SPDE mesh for modelling 

A  <-  inla.spde.make.A(mesh, loc = points)

class(A)

spde  <-  inla.spde2.matern(mesh, alpha = 2)

# Building study specific spde matrix

sigma0  = 1
size  =  min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2]))))
range0  =  size / 5
kappa0 = sqrt(8) / range0
tau0 = 1 / (sqrt(4 * pi) * kappa0 * sigma0)
spde = inla.spde2.matern(mesh, 
  B.tau = cbind(log(tau0), -1, +1), 
  B.kappa = cbind(log(kappa0), 0, -1),  
  theta.prior.mean = c(0, 0), 
  theta.prior.prec = c(0.1, 1) )