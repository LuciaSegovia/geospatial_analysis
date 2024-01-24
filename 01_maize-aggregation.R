################################################################################
#
#           Calculate the mean grain Se conc. 
#          of the admin units for each cluster 
#         for future use and spatial modelling.
#             
#
#
################################################################################

# Cleaning the environment ----
rm(list=ls())

# Loading libraries and functions -----

#library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Data: Shapefiles ----

## Admin Boundaries for Malawi ----

# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
ea_admin <- st_read(here::here( "data", "inter-output", 
                                "boundaries", "mwi_admbnda_adm4_nso.shp")
                    
# Districts
dist_bnd  <- st_read(here::here( "data",
                                "mwi-boundaries",
                                "mwi_adm_nso_hotosm_20230329_shp", 
                                "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))

## Buffers for Malawi ----


# Cluster's EA info

plasma.df <- readRDS(here::here("data", "inter-output", 
                                "dhs_se_gps_admin.RDS"))

names(plasma.df)


# Maize data ----

## Observed maize ----

# Observed maize data 
maize.df <- readRDS(here::here("data", "inter-output", 
                             "mwi_maize-se-raw_admin.RDS"))


# Merge cluster's EA and observed maize

maize.df %>% right_join(., cluster.df)


