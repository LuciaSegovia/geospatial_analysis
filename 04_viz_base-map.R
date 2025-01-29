
# Loading libraries and functions

library(dplyr) # data wrangling 
#library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(scales) # visualisation - colours
library(paletteer) # visualisation - ggplot2 add-on colours
library(ggridges)  # visualisaiton - ggplot2 add-on for ridges
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Loading Shapefiles -----

# Admin Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("data",
                              "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this


# Base map: Malawi ------

# Loading data ----
# Removing lakes from boundaries dataset
# Selecting only variables that are interesting 
# EA code, EA area, TA code, district & geometry)
ea_admin <- ea_bnd %>% filter(!grepl("lake", DISTRICT,
                                     ignore.case = TRUE)) %>% 
  dplyr::select(c(4, 10, 11, 17, 18))


malawi_bnd_lakes <- st_union(ea_bnd) # Aggregate boundaries the whole country (with lakes)
malawi_bnd <- st_union(ea_admin) # Aggregate boundaries the whole country


# The Map ----
# Base map (EAs)
base_map <- tm_shape(ea_admin) +    
  tm_polygons(fill = "white", 
              col = "#666666", col_alpha = 0.3, lwd = 0.2) +
  # Land boundaries
  tm_shape(malawi_bnd) +   
  tm_borders(col = "#666666", fill_alpha = 0.6, lwd = 0.5) +
  # Land/ Lake boundaries
  tm_shape(malawi_bnd_lakes) +
  tm_borders(col = "black", fill_alpha = 0.6, lwd = 0.5) 