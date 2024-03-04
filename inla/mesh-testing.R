
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

# Locations
coord <- cbind(plasma_se$Longitude, plasma_se$Latitude) # using the sampling locations 

MeshA <- inla.mesh.2d(jitter(coord), max.edge = c(20, 40))
MeshB <- inla.mesh.2d(coord, max.edge = c(20, 40)) # Same unit as coordinates
MeshC <- inla.mesh.2d(coord, max.edge = c(10, 20))
MeshD <- inla.mesh.2d(loc = coord, max.edge = c(.5, 3), cutoff = c(0.001)) # More "intensive" due to smaller size of triangles
MeshE <- inla.mesh.2d(loc = coord, max.edge = c(.4, 1), cutoff = c(0.1)) # More "intensive" due to smaller size of triangles

Mesh <- MeshD # this felt the best

plot(MeshA)
plot(MeshB)
plot(MeshC)
plot(MeshE)

#plot(Mesh)

# Non convex

mwidomain <- inla.nonconvex.hull(coord,  concave=0.15, resolution=c(200,500))
mwidomain <- inla.nonconvex.hull(coord,  concave=0.03, resolution=c(200,500))
mwidomain <- inla.nonconvex.hull(coord,  concave=0.03, resolution=c(200,350))
#mwidomain <- inla.nonconvex.hull(coord,  concave=0.15, convex=0.03, resolution=c(200,500))
#mwidomain <- inla.nonconvex.hull(coord, 0.03, resolution=c(120,300))

#mesh <- inla.mesh.2d(boundary=mwidomain, max.edge=c(.7,.7), cutoff=0.35, offset=c(-0.05, -0.05))
mesh <- inla.mesh.2d(boundary=mwidomain, max.edge = c(.4, 1), cutoff = c(0.1))

plot(mesh, asp=1, main='')
lines(mwi_bnd, col=3)
points(coord, col = "red", pch = 2)


