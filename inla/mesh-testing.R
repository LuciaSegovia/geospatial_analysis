
# Loading libraries
library(INLA) # Modelling (RINLA)
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling

# Loading the data

file <- grep("plasma", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)
i=1
plasma_se <- readRDS(here::here("data", "inter-output", "model", file[i]))

# Locations
coord <- cbind(plasma_se$Longitude, plasma_se$Latitude) # using the sampling locations 

MeshA <- inla.mesh.2d(jitter(coord), max.edge = c(20, 40))
MeshB <- inla.mesh.2d(coord, max.edge = c(20, 40))
MeshC <- inla.mesh.2d(coord, max.edge = c(10, 20))

Mesh <- MeshB # this felt the best

plot(MeshA)

plot(MeshB)

plot(MeshC)

points(coord, col = "red", pch = 2)


