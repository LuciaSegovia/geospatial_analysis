
# Generating visuals for the manuscript


# Cleaning the enviroment
rm(list=ls())

# Loading libraries and functions

library(dplyr) # data wrangling 
library(tidyr) # data tidying
#library(plyr) # weighted data analysis
library(purrr) # functions and programming
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

# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
ea_admin <- st_read(here::here( "data", "inter-output", 
                                "boundaries", "mwi_admbnda_adm4_nso.shp"))
# Changing variable region class to factor
ea_admin$region <- as.factor(ea_admin$region)

# Cluster's EA info

plasma.df <- readRDS(here::here("data", "inter-output", 
                                "dhs_se_gps_admin.RDS")) 
names(plasma.df)

# Number of WRA with plasma values
length(unique(plasma.df$unique_id[!is.na(plasma.df$selenium)]))

cluster.df <- plasma.df %>% 
  distinct(survey_cluster1, EACODE, ADM2_PCODE, ADM2_EN, urbanity)

EAselected <- unique(cluster.df$EACODE)
cluster.df$survey_cluster1 <- as.factor(cluster.df$survey_cluster1)

# Cluster location

geocluster <- plasma.df %>% select(survey_cluster1, Longitude, Latitude) %>% 
  distinct() %>%  st_as_sf(. , coords =c("Longitude", "Latitude"),
                           crs = "EPSG:4326")

# Variables and labels ----

# Discrete variables (factors) from number to text
lab_region <- c(`1` = "Northern", `2` = "Central", `3` = "Southern")
lab_reside <- c(`1` = "Urban", `2` = "Rural")
lab_malaria <- c(`1` = "Positive", `2` = "Negative")

## Checking maps & locations

tm_shape(ea_admin) +    
  tm_polygons(col = "white" , #tested border.alpha 0.3
              border.col = "red", border.alpha = 0.5 , lwd = 0.2
  ) +
  tm_shape(dist_bnd) +   #tested alpha 0.5
  tm_polygons(col = "#666666", alpha = 0.4, lwd = 0.5) +
   tm_shape(geocluster) +
  tm_symbols(col = "yellow", size = 0.5)

# Maps! ------

## Base map: Malawi ------

mwi_lakes <- ea_bnd %>% filter(grepl("lake", DISTRICT,
                                                  ignore.case = TRUE)) %>% 
  select(c(4, 10, 11, 17, 18))

malawi_bnd_lakes <- st_union(ea_bnd) # Aggregate boundaries the whole country (with lakes)
malawi_bnd <- st_union(ea_admin) # Aggregate boundaries the whole country

# The Map:
# Base map (EAs)
base_map <- tm_shape(ea_admin) +    
  tm_polygons(col = "white" , border.alpha = 0 # , 
           #   border.col = "#666666", border.alpha = 0.3, lwd = 0.2
             ) +
  # Land boundaries
 # tm_shape(malawi_bnd) +   
  tm_shape(dist_bnd) +
    tm_borders(col = "#666666", alpha = 0.8, lwd = 0.5) +
  # Colouring lakes
  tm_shape(mwi_lakes) +
  tm_polygons(col = "#253DA1", border.alpha = 0) +
  # Land/ Lake boundaries
  tm_shape(malawi_bnd_lakes) +
  tm_borders(col = "black", alpha = 0.6, lwd = 0.5) 


# Fig. 2: Spatial aggregation of maize Se conc. 

# Aggregated maize Se conc. files (from 01_maize-aggregation.R)
file <- grep("pred-maize", list.files(here::here("data", "inter-output", "aggregation")), 
             value = TRUE)

#Buffer aggregations file
buff.dist <- na.omit(unique(str_extract(list.files(here::here("data", "inter-output", "boundaries")),
                                        "[:digit:]{2}")))

# Loading aggregated maize data
i =3
maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               file[i])) 

sum(is.na(is.na(maize.df$survey_cluster1)))

names(maize.df)
maize.df$survey_cluster1 <- as.factor(maize.df$survey_cluster1)
maize.df$Se_mean <- as.numeric(maize.df$Se_mean)

map <- list()

for(i in 1:length(file)){
  
  maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                                 file[i])) 
  
  sum(is.na(is.na(maize.df$survey_cluster1)))
  
  names(maize.df)
  maize.df$survey_cluster1 <- as.factor(maize.df$survey_cluster1)
  maize.df$Se_mean <- as.numeric(maize.df$Se_mean)
  
if (i < 4) {

buffer  <- st_read(here::here("data", "inter-output",
                                "boundaries", 
                                paste0("mwi_gps-buffer", buff.dist[i], ".shp"))) %>% 
    rename(survey_cluster1 = "srvy_c1")
 
 buffer$survey_cluster1 <- as.factor(buffer$survey_cluster1)
 
 geodata.df <- maize.df %>% 
   left_join(., buffer) %>% 
   select(survey_cluster1, Se_mean, geometry) %>% 
   filter(!is.na(survey_cluster1)) %>% 
   st_as_sf(., crs = "EPSG:4326") 
 
map[[i]] <- base_map +
  tm_shape(geodata.df) +
  tm_polygons(col ="Se_mean",alpha = 0.5, border.col = "black", border.alpha = 0.5, lwd = 0.1) +
  tm_legend(legend.outside=T, legend.outside.position="right", legend.text.size = 1)

} else if (i == 4) {

geodata.df <- maize.df %>% 
  left_join(., cluster.df) %>% 
  left_join(., ea_admin) %>% 
  select(survey_cluster1, Se_mean, geometry) %>% filter(!is.na(survey_cluster1)) %>% 
  # select(ADM2_EN, Se_mean, geometry) %>% distinct() %>% 
  st_as_sf(., crs = "EPSG:4326") # Transform into spatial obj

map[[i]] <- base_map +
  tm_shape(geodata.df) +
   tm_polygons(col ="Se_mean", border.col = "black", border.alpha = 0.5, lwd = 0.1) +
  tm_legend(legend.outside=T, legend.outside.position="right", legend.text.size = 1)

} else {
  
  geodata.df <- maize.df %>%
    left_join(., dist_bnd) %>% 
    select(ADM2_EN, Se_mean, geometry) %>% distinct() %>% 
    st_as_sf(., crs = "EPSG:4326") # Transform into spatial obj
  
  map[[i]] <- base_map +
    tm_shape(geodata.df) +
    tm_polygons(col ="Se_mean", border.col = "black", border.alpha = 0.5, lwd = 0.1) +
    tm_legend(legend.outside=T, legend.outside.position="right", legend.text.size = 1)
  
}

}




base_map +
  tm_shape(geodata.df) +
  #tm_polygons(col ="firebrick4", border.col = "black", border.alpha = 0.3) 
  tm_polygons(col ="survey_cluster1",  border.alpha = 0, legend.show = FALSE) 


tm_shape(dist_bnd) +
  tm_polygons(col ="ADM2_EN", legend.show = FALSE)


# Table 1.  maize Se conc. ---------

## Loading the data

file <- grep("plasma-pred", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-mwi-lakes.RDS"))


name <- str_extract(file, "maize-([:alnum:]+)")


# saving all the cleaned FCTs/FCDBs into one single object (data.frame)

data.df <- here::here("data", "inter-output", "model", file) %>% 
  map(~readRDS(.))

data.df[[5]] %>% names()


for(i in 1:length(name)){

names(data.df[[i]])[grepl("Se", names(data.df[[i]]))] <- paste0(name[i],"_", grep("Se", names(data.df[[i]]), value = TRUE))
  
}

   # map(~rename_with(~paste0(name, .x), starts_with("Se_")))
 # lmap(~rename(name = "Se_mean"))
 # lmap(~set_names(1, name))


      
summary.table <- data.df %>% reduce(left_join) %>% 
  select(survey_cluster1, household_id1,
                   region,
                   wealth_quintile, urbanity, 
                   AGE_IN_YEARS, crp, agp, starts_with(name)) %>% 
  # Joining the variable distance to inland water body
  left_join(., dist) 

# Saving the summary table

summary.table %>% na.omit() %>% 
  write.csv(., here::here("output", "summary-input_v.1.0.0.csv"),
            row.names = FALSE)

## Error bars and maize Se conc. ---------

summary.table %>% select(survey_cluster1, ends_with("mean"),ends_with("sd"), region) %>% 
  pivot_longer(cols = c(ends_with("mean"), ends_with("sd")), 
                               names_to = "maize_aggr",
                               values_to = "Se") %>%   distinct() %>% 
  separate_wider_delim(maize_aggr, "_", 
              names = c("aggregation", "value_type"), too_many = "merge") %>% 
  pivot_wider(names_from = value_type, values_from = Se) %>% 
  ggplot(aes( as.factor(survey_cluster1), Se_mean, colour = aggregation)) +
  geom_point( position=position_dodge(0.75))  + 
  geom_errorbar(aes(ymin=Se_mean-Se_sd, ymax=Se_mean+Se_sd), width=.2,
                position=position_dodge(0.75)) +
  theme_light() +   coord_flip() + 
#  facet_grid(rows = vars(region),
  facet_wrap(~region, 
             labeller = as_labeller(lab_region),
           scales = "free_y") + 
  labs(x = "",
    y = expression(paste("Maize Se conc. (mg  ",  Kg^{-1}, ")"))) + 
#  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10)) +
  theme(legend.position = "top")
   
    
    
         
# covariates selection

covar <- c("Se_mean", "wealth_quintile", "urbanity", 
           # "Malaria_test_result", "BMI",  
           "AGE_IN_YEARS", "crp", "agp", "dist_to_lake")




