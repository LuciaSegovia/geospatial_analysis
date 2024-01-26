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
                                "boundaries", "mwi_admbnda_adm4_nso.shp"))
                    
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

cluster.df <- plasma.df %>% 
  distinct(survey_cluster1, EACODE, ADM2_PCODE, ADM2_EN, urbanity)

# Maize data ----

## Observed maize ----

# Observed maize data 
maize.df <- readRDS(here::here("data", "inter-output", 
                             "mwi_maize-se-raw_admin.RDS"))


# Merge cluster's EA and observed maize

maize_cluster <- maize.df %>% right_join(., cluster.df) 

# Calculating the maize grain Se conc per cluster
maize_cluster <- maize_cluster %>% # filter(!is.na(Se_raw)) %>% 
  group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(Se_raw, na.rm = TRUE), 
            Se_sd = mean(Se_raw, na.rm = TRUE), 
            Se_median = median(Se_raw, na.rm = TRUE), 
            Se_iqr = IQR(Se_raw, na.rm = TRUE), 
            Se_n = n()) 

# Getting the cluster with missing values 
miss <-  unique(maize_cluster$survey_cluster1[is.na(maize_cluster$Se_mean)])

# Selecting only the EAs that were included in the missing clusters
 ea_cluster <- cluster.df %>% filter(survey_cluster1 %in% miss) %>% 
   distinct(EACODE) %>% left_join(., ea_admin) %>% st_as_sf()
 
 # Transforming maize data.frame into a spatial object (geometry) 
 geomaize.df <- maize.df  %>% select(1:25,"Longitude", "Latitude" ) %>% 
   st_as_sf(coords =c("Longitude", "Latitude"),
                         crs = "EPSG:4326")
 
 dim(geomaize.df) # 1689 maize GeoNut 
 

m=230

st_join(geomaize.df, ea_cluster, st_is_within_distance,  
        dist = units::set_units(m, "m")) %>% filter(!is.na(EACODE))


dim(missing_maize)
sum(is.na(missing_maize$EACODE))
sum(is.na(geomaize_ea.df$survey_cluster1))




for(i in 1:nrow(ea_cluster)){
  
  m <- 230
  
  if(sum(ls()=="missing")==0){
    missing <- test
  }
  
  missing <- rbind(missing, test)
  
  print(nrow(missing))
  
repeat {

m <- m + 20 

  test <- st_join(geomaize.df, ea_cluster[i,], st_is_within_distance,  
          dist = units::set_units(m, "m")) %>% 
          filter(!is.na(EACODE))%>% 
          mutate(dist_in_m = m)
  
  print(i)
  print(m)
  
  if(nrow(test)>0) { break}  
    
}

}

# Saving output of the loop
#missing %>% st_drop_geometry() %>%
 # saveRDS(., here::here("data", "inter-output", "filling-missing-Se-observed-maize.RDS"))

length(unique(missing$EACODE))
length(unique(ea_cluster$EACODE))

hist(missing$dist_in_m)
hist(missing$Se_raw)

cluster.df  %>% 
  filter(survey_cluster1 %in% miss) %>% left_join(., missing) %>% 
  #filter(is.na(Se_raw))
  filter(survey_cluster1 == "777") %>% 
  select(survey_cluster1, urbanity,EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
 group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(Se_raw, na.rm = TRUE), 
            Se_sd = sd(Se_raw, na.rm = TRUE), 
            dist_mean = mean(dist_in_m, na.rm = TRUE), 
            dist_sd = sd(dist_in_m, na.rm = TRUE))

cluster.df  %>% 
  filter(survey_cluster1 %in% miss) %>% left_join(., missing) %>% 
  arrange(survey_cluster1, dist_in_m) %>% 
  select(survey_cluster1, urbanity,EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
  group_by(survey_cluster1) %>% 
  slice_min(dist_in_m, n=1)

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
  select(survey_cluster1, urbanity,EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
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
