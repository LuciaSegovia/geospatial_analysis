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
options(scipen=999)

# Loading libraries and functions -----

#library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(stringr) # string data manipulation 
library(ggplot2) # visualisation
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Loading data: Shapefiles ----

## Admin Boundaries for Malawi ----

# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
ea_admin <- st_read(here::here( "data", "inter-output", 
                                "boundaries", "mwi_admbnda_adm4_nso.shp"))
# Changing variable region class to factor
ea_admin$region <- as.factor(ea_admin$region)

# Districts
dist_bnd  <- st_read(here::here( "data",
                                 "mwi-boundaries",
                                 "mwi_adm_nso_hotosm_20230329_shp", 
                                 "mwi_admbnda_adm2_nso_hotosm_20230329.shp"))
dist_bnd <- st_make_valid(dist_bnd) # Check this

# National parks

parks  <-  st_read(here::here("..", "PhD_geospatial-modelling", "data",
                              "mwi-boundaries", "protected_areas_geo",
                              "protected_areas_geo.shp"))


# Cluster's EA info

plasma.df <- readRDS(here::here("data", "inter-output", 
                                "dhs_se_gps_admin.RDS")) 
names(plasma.df)

# Number of WRA with plasma values
length(unique(plasma.df$unique_id[!is.na(plasma.df$selenium)]))

cluster.df <- plasma.df %>% 
  distinct(survey_cluster1, EACODE, ADM2_PCODE, ADM2_EN, urbanity)

EAselected <- unique(cluster.df$EACODE)

# Cluster area 

geo.clust <- cluster.df %>% left_join(., ea_admin) %>% 
  distinct(survey_cluster1, geometry) %>% 
  st_as_sf() 

## Calculating the area of the EA groups ------------
geo.clust <-  geo.clust %>% 
  group_by(survey_cluster1) %>%
 # dplyr::summarise(area = sum(AREA_KM))
  dplyr::summarise()

area.clust <-  cluster.df %>% left_join(., ea_admin) %>%
  dplyr::select(survey_cluster1,  AREA_KM, 
         geometry) %>%
  group_by(survey_cluster1) %>%
  dplyr::summarise(area = sum(AREA_KM))

summary(area.clust)

geo.clust$area_cal <- st_area(geo.clust) #m^2

summary(geo.clust$area_cal)

geo.clust$survey_cluster1 <- as.character(geo.clust$survey_cluster1)

# Checking the large EAs

largeEA <- geo.clust %>% st_drop_geometry() %>% 
  mutate(area=as.numeric(area_cal/1000000)) %>% filter(area>314) %>% 
  pull(survey_cluster1)

tm_shape(ea_admin) +
  tm_polygons() +
  tm_shape(geo.clust %>% filter(survey_cluster1 %in% largeEA))+
  tm_polygons(col = "survey_cluster1") +
  tm_shape(parks)+
  tm_borders(col = "darkgreen",lwd = 2)


## Distritc area

area.dist <-  ea_admin %>% st_drop_geometry() %>% 
  dplyr::select(ADM2_EN,  AREA_KM) %>% distinct() %>% 
  group_by(ADM2_EN) %>%
  dplyr::summarise(area = sum(AREA_KM))

summary(area.dist)

# Predicted Maize Se conc.  ----

# Loading the data
# Predicted Se conc. (predicted in 01_maize-model.R)
predmaize.df <- read.csv(here::here("data", "OK",
                                    "2024-05-03Se_raw_OK_expmaize.csv"))  %>%  
  dplyr::rename(predSe = "Zhat_exp")
names(predmaize.df)

# Getting only cluster location (to avoid duplicates), 
# renaming buffer as geometry for converting into spatial object
geopredmaize.df <-  st_as_sf( predmaize.df , coords =c("Longitude", "Latitude"),
                              crs = "EPSG:4326")

### Cluster ----

## Checking the EAs of the HHs with predicted maize

bnd_reduced <- ea_admin %>% filter(EACODE %in% EAselected)

geopred_ea <-  st_join(geopredmaize.df, bnd_reduced)

sum(!is.na(geopred_ea$ADM2_EN))


# Calculating the maize grain Se conc per cluster
predmaize_cluster <- geopred_ea %>% 
  st_drop_geometry() %>% filter(!is.na(ADM2_EN)) %>% 
  right_join(., cluster.df) %>%  filter(!is.na(predSe)) %>% 
  group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(predSe, na.rm = TRUE), 
            Se_sd = sd(predSe, na.rm = TRUE), 
            Se_median = median(predSe, na.rm = TRUE), 
            Se_iqr = IQR(predSe, na.rm = TRUE), 
            Se_n = n()) 


# Saving observed maize grain Se concentration per cluster (smallest admin boundary).
# saveRDS(predmaize_cluster, here::here("data", "inter-output", "aggregation", 
#                                "pred-maize-cluster_v2.0.0.RDS"))


### District -----

# Spatially join both dataset

dist_maize <- st_join(geopredmaize.df, dist_bnd) 

Se <- "predSe"
# Se <- "Zhat_exp"

dist_maize <- dist_maize %>% 
  st_drop_geometry() %>% filter(!is.na(ADM2_EN)) %>% 
  group_by(ADM2_EN) %>% 
  summarise(Se_mean = mean(!!sym(Se)), 
            Se_sd = sd(!!sym(Se)), 
            Se_median = median(!!sym(Se)), 
            Se_iqr = IQR(!!sym(Se)), 
            Se_n = n()) %>% 
  arrange(ADM2_EN) 

# Saving observed maize grain Se concentration per district.
# saveRDS(dist_maize, here::here("data", "inter-output", "aggregation", 
# "pred-maize-district_v2.0.0.RDS"))

### Region -----

# Aggregate boundaries the three country 

region_bnd <- dist_bnd %>% group_by(ADM1_EN) %>% summarise()

tm_shape(region_bnd) +
  tm_polygons(col = "ADM1_EN")

# Spatially join both dataset

region_maize <- st_join(geopredmaize.df, region_bnd) 

Se <- "predSe"
# Se <- "Zhat_exp"

region_maize <- region_maize %>% 
  st_drop_geometry() %>% filter(!is.na(ADM1_EN)) %>% 
  group_by(ADM1_EN) %>% 
  summarise(Se_mean = mean(!!sym(Se)), 
            Se_sd = sd(!!sym(Se)), 
            Se_median = median(!!sym(Se)), 
            Se_iqr = IQR(!!sym(Se)), 
            Se_n = n()) %>% 
  arrange(ADM1_EN) 

# Saving observed maize grain Se concentration per district.
# saveRDS(region_maize, here::here("data", "inter-output", "aggregation", 
# "pred-maize-region_v2.0.0.RDS"))

### Buffers  -----

# If different buffers what to be generated and tested, 
# The function buffer_generator() can be used.
# Note that for our case, we would like the buffer to be 
# max around the biggest district
# if buffer (circular) area = pi*r^2 
# sqrt((max(st_area(dist_bnd))/10^6)/pi) # around 60km

# Calculating buffer
for(i in c(10, 15, 20, 25, 30, 40, 50, 60)){
  print(pi*i^2)
  }

(buff.dist <- na.omit(unique(stringr::str_extract(list.files(here::here("data", 
                                                                        "inter-output", "boundaries", "buffer")),
                                                  "[:digit:]{2}"))))

geodata.df <- geopredmaize.df

Se <- grep("Se", names(geodata.df), value = TRUE)
#Se <- grep("exp", names(geodata.df), value = TRUE)

for(i in 1:length(buff.dist)){
  
  buffer  <- st_read(here::here("data", "inter-output",
                                "boundaries", "buffer",
                                paste0("mwi_buffer", buff.dist[i], ".shp"))) %>% 
    dplyr::rename(survey_cluster1 = "srvy_c1")
  
  maize_buff <- st_join(geodata.df, buffer) 
  
  if(sum(is.na(maize_buff[,Se]))>0){
    print("Error in the data merging")
  }
  
  maize_buff %>% st_drop_geometry() 
  group_by(survey_cluster1) %>% 
    summarise(Se_mean = mean(!!sym(Se)), 
              Se_sd = sd(!!sym(Se)), 
              Se_median = median(!!sym(Se)), 
              Se_iqr = IQR(!!sym(Se)), 
              Se_n = n()) %>% 
    arrange(Se_n) %>% 
    saveRDS(., here::here("data", "inter-output",   "aggregation",  
                          paste0("pred-maize-buffer", 
                                 buff.dist[i], "_v2.0.0.RDS")))
  
}


# Visual checks ----

i = 1

buffer_min  <- st_read(here::here("data", "inter-output",
                                  "boundaries", "buffer",
                                  paste0("mwi_buffer", buff.dist[i], ".shp"))) %>% 
  rename(survey_cluster1 = "srvy_c1")

i = 6

buffer_max  <- st_read(here::here("data", "inter-output",
                                  "boundaries", "buffer",
                                  paste0("mwi_buffer", buff.dist[i], ".shp"))) %>% 
  rename(survey_cluster1 = "srvy_c1")


tm_shape(ea_admin) +
  tm_polygons() +
  tm_shape(buffer_min)+
  tm_borders(col = "red") +
  tm_shape(buffer_max)+
  tm_borders(col = "green")


data.df <- readRDS(here::here("data", "inter-output",   "aggregation",  
                              paste0("pred-maize-buffer", 
                                     buff.dist[i], "_v2.0.0.RDS"))) %>% 
  filter(!is.na(survey_cluster1))

data.df  <- data.df %>% left_join(., buffer_min) %>% st_as_sf() 

tm_shape(ea_admin) +
  tm_polygons() +
  tm_shape(data.df)+
  tm_polygons(col = "Se_mean") 


cluster.df  %>% 
  filter(survey_cluster1 %in% miss) %>% left_join(., missing) %>% 
  arrange(survey_cluster1, dist_in_m) %>% 
  select(survey_cluster1, urbanity,EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
  ggplot(aes(Se_raw, fill=as.character(survey_cluster1))) + 
  geom_histogram() + scale_fill_discrete(name = "Cluster") +
  theme_bw() + facet_wrap(~survey_cluster1) 


cluster.df  %>% 
  filter(survey_cluster1 %in% miss) %>% left_join(., missing) %>% 
  arrange(survey_cluster1, dist_in_m) %>% 
  select(survey_cluster1, urbanity, EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
  ggplot(aes(Se_raw, dist_in_m, colour=as.character(ADM2_EN))) + 
  geom_point(aes(size=Se_raw))  +  
  labs(size="Maize Se conc.(mcg/kg)", colour="District") +
  theme_bw() + facet_wrap(~survey_cluster1) 

maize.df %>% right_join(., cluster.df) %>% 
  mutate_at("survey_cluster1", as.character) %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(survey_cluster1, Se_raw)) + geom_boxplot() +
  facet_wrap(~region, labeller = as_labeller(c(`1` = "Northern", 
                                               `2` = "Central", 
                                               `3` = "Southern")),
             scales = "free_x") 

tm_shape(ea_admin) +
  tm_polygons(col = "white", 
              border.col = "#666666", border.alpha = 0.3, lwd = 0.2) +
  #  tm_shape(ea_cluster) +
  #  tm_polygons(col ="#138e61", border.col = "black", border.alpha = 0.3) +
  tm_shape(missing) +
  tm_symbols(col = "red", alpha = 0.01) 

