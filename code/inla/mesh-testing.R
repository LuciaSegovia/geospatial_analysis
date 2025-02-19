
# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling

# Loading Shapefiles 

# Admin Boundaries for Malawi 

# Districts
mwi_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm0_nso_hotosm_20230329.shp"))

table(sf::st_is_valid(mwi_bnd))
mwi_bnd <- st_make_valid(mwi_bnd) # Check this

# Loading the data

file <- grep("plasma", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)
i=1
plasma_se <- readRDS(here::here("data", "inter-output", "model", file[i]))

# Mesh structure -------
# Example from: (Lindgren and Rue, 2015, p. 10)
m  <-  50
points  <-  matrix(runif(m * 2), m, 2)
mesh  <-  inla.mesh.2d( loc = points,  # coordinates
                        cutoff = 0.05,    # (optional) to avoid many small triangles in clustered locations
                        offset = c(0.1, 0.4),  # (optional) inner and outer extensions around the data locations
                        max.edge = c(0.05, 0.5))  # maximum allowed triangle edge lengths in the inner domain and in the outer extension

plot(mesh)

# Our data -------

## Locations
coord <- cbind(plasma_se$Longitude, plasma_se$Latitude) # using the sampling locations 

# Exploring the coordinates
min(coord)
max(coord)

## Manual testing based on
# Moraga et al. 2019
# https://www.paulamoraga.com/book-geospatial/sec-geostatisticaldatatheory.html
#using the summary distance to approximate the mesh values
summary(dist(coord))

# Generating the mesh based on the point locations
mesh <- inla.mesh.2d(loc = coord, max.edge = c(.3, .7), cutoff = c(0.0001)) 

# Visualising
plot(mesh, asp=1, main='')
points(coord, col = "red", pch = 1)

## Testing: Generating several mesh
MeshA <- inla.mesh.2d(jitter(coord), max.edge = c(20, 40))
MeshB <- inla.mesh.2d(coord, max.edge = c(20, 40)) # Same unit as coordinates
MeshC <- inla.mesh.2d(coord, max.edge = c(10, 20))
MeshD <- inla.mesh.2d(loc = coord, max.edge = c(.5, 3), cutoff = c(0.001)) # More "intensive" due to smaller size of triangles
MeshE <- inla.mesh.2d(loc = coord, max.edge = c(.4, 1), cutoff = c(0.1)) 
MeshF <- inla.mesh.2d(loc = coord, max.edge = c(.5, 3), cutoff = c(0.1)) # Combination between the two
MeshG <- inla.mesh.2d(loc = coord, max.edge = c(.3, .7), cutoff = c(0.0001)) # Seemed the best
MeshH <- inla.mesh.2d(loc = coord, max.edge = c(.3, .7), cutoff = c(0.001)) 

# Plotting the mesh
plot(MeshA)
plot(MeshB)
plot(MeshC)
plot(MeshD)
plot(MeshE)

Mesh <- MeshG # this felt the best

plot(Mesh)

# Now compared with Krainski and Rue,  2017.
# https://inla.r-inla-download.org/r-inla.org/doc/vignettes/SPDEhowto.pdf
# Using the range as reference (from the variogram in "01_plasma-variogram.R")
# Range (downscale)
r <- 110/100
mesh <- inla.mesh.2d(coord, cutoff=r/10,
                     max.edge=c(r/4, r/2), offset=c(r/2, r))
plot(mesh, asp=1); points(coord, col='red')

# Similar results but better...

# Non convex

# Generating the boundaries of Malawi based on the point locations
mwidomainA <- inla.nonconvex.hull(coord,  concave=0.15, resolution=c(200,500))
mwidomainB <- inla.nonconvex.hull(coord,  concave=0.03, resolution=c(200,500))
mwidomainC <- inla.nonconvex.hull(coord,  concave=0.03, resolution=c(200,350))
#mwidomain <- inla.nonconvex.hull(coord,  concave=0.15, convex=0.03, resolution=c(200,500))
#mwidomain <- inla.nonconvex.hull(coord, 0.03, resolution=c(120,300))
mwidomain <- mwidomainC # The best one from these ones

#mesh <- inla.mesh.2d(boundary=mwidomain, max.edge=c(.7,.7), cutoff=0.35, offset=c(-0.05, -0.05))
mesh <- inla.mesh.2d(boundary=mwidomain, max.edge = c(.4, 1), cutoff = c(0.1))


plot(mesh, asp=1, main='')
lines(mwi_bnd, col=3)
points(coord, col = "red", pch = 2)


# mesh <- inla.mesh.2d(boundary=mwidomain, max.edge = c(.4, 1), cutoff = c(0.1))


# NOT WORKING: Inner dist = max(coord), outter dist double inner dist and 
# distance in triangles 1/5 of range (Moraga et al., 2021)

# mesh <- inla.mesh.2d(loc = coord,  
#                      max.edge = c(17,34),
#                      cutoff = 24)


#mesh <- inla.mesh.2d(loc = coord, 
#                      max.edge = c(.5, 3), 
#                      cutoff = c(0.03))

mesh <- inla.mesh.2d(loc = coord, 
                     max.edge = c(.5, 3), 
                     cutoff = c(0.001))

# Best one in Righetto et al., 2020 (simulated data)
#mesh2 <- inla.mesh.2d(loc = geom, max.edge = c(0.09, 0.3), 
#     cutoff = c(0.03))


