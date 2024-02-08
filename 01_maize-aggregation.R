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
library(stringr) # string data manipulation 
library(ggplot2) # visualisation
library(sf) # spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


# Data: Shapefiles ----

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


# Cluster's EA info

plasma.df <- readRDS(here::here("data", "inter-output", 
                                "dhs_se_gps_admin.RDS")) 
names(plasma.df)

cluster.df <- plasma.df %>% 
  distinct(survey_cluster1, EACODE, ADM2_PCODE, ADM2_EN, urbanity)

EAselected <- unique(cluster.df$EACODE)


# Maize data ----

## Observed Se conc. ----

# Observed maize data (from 00_cleaning-location.R)
maize.df <- readRDS(here::here("data", "inter-output", 
                             "mwi_maize-se-raw_admin.RDS"))

maize.df$ID[maize.df$survey == "Chilimba"] <- paste0("MW_Ch", 1:86)
maize.df$dist_in_m[maize.df$survey == "Chilimba"] <- paste0("MW_Ch", 1:86)
# Changing 0 to min value (for log transf.)
maize.df$Se_raw[maize.df$Se_raw == 0] <- 0.00269
names(maize.df)
class(maize.df$region)

# Transforming maize data.frame into a spatial object (geometry) 
geomaize.df <- maize.df  %>% select(1:25,"Longitude", "Latitude" ) %>% 
  st_as_sf(coords =c("Longitude", "Latitude"),
           crs = "EPSG:4326")

dim(geomaize.df) # 1689 maize GeoNut 

### Cluster prep ----
# TODO: Check if clusters with highest sd are the same for 
# plasma and maize.

# Merge cluster's EA and observed maize
maize_cluster <- maize.df %>% right_join(., cluster.df) 

# Calculating the maize grain Se conc per cluster
maize_cluster <- maize_cluster %>% # filter(!is.na(Se_raw)) %>% 
  group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(Se_raw, na.rm = TRUE), 
            Se_sd = sd(Se_raw, na.rm = TRUE), 
            Se_median = median(Se_raw, na.rm = TRUE), 
            Se_iqr = IQR(Se_raw, na.rm = TRUE), 
            Se_n = n()) 

# Getting the cluster with missing values 
miss <-  unique(maize_cluster$survey_cluster1[is.na(maize_cluster$Se_mean)])

# Selecting only the EAs that were included in the missing clusters
 ea_cluster <- cluster.df %>% filter(survey_cluster1 %in% miss) %>% 
   distinct(EACODE) %>% left_join(., ea_admin) %>% st_as_sf()
 
# Checking min. distance to the closest sample 

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

### Cluster  ----

# Loading the data
#missing <- readRDS(here::here("data", "inter-output", "filling-missing-Se-observed-maize.RDS"))

class(missing$region)
# Changing variable region class to factor
missing$region <- as.factor(missing$region)

head(missing)
missing$survey[is.na(missing$ID)]

missing$ADM2_EN[duplicated(missing$Se_raw)]

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
            dist_sd = sd(dist_in_m, na.rm = TRUE)) %>% View()

missing_clusters <- cluster.df  %>% 
  filter(survey_cluster1 %in% miss) %>% left_join(., missing) %>% 
  arrange(survey_cluster1, dist_in_m) %>% 
#  select(ID, survey_cluster1, urbanity,EACODE,ADM2_EN, dist_in_m, Se_raw) %>% 
  group_by(survey_cluster1) %>% 
  slice_min(dist_in_m, n=1)

# Checking that the Se values has being used for other clusters (duplicated)

test <- missing_clusters$ID

maize.df %>% right_join(., cluster.df) %>% 
   filter(!is.na(Se_raw)) %>% filter(ID %in% test)

# We can paste those values into the dataset and calculate the mean. 

maize.df %>% right_join(., cluster.df) %>% 
  filter(!is.na(Se_raw)) %>%  bind_rows(., missing_clusters) %>% 
  filter(!is.na(Se_raw)) %>% count(survey_cluster1) %>% 
  arrange(desc(n))
  
cluster_maize <- maize.df %>% right_join(., cluster.df) %>% 
   bind_rows(., missing_clusters) %>% 
  filter(!is.na(Se_raw)) %>% 
group_by(survey_cluster1) %>% 
   summarise(Se_mean = mean(Se_raw), 
            Se_sd = sd(Se_raw), 
            Se_median = median(Se_raw), 
            Se_iqr = IQR(Se_raw), 
            Se_n = n(), 
            Dist_mean = ifelse(Se_n>1, 
              mean(dist_in_m, na.rm = TRUE), dist_in_m)) %>% 
  arrange(Se_n)

hist(cluster_maize$Se_mean)
hist(cluster_maize$Se_sd)
hist(cluster_maize$Dist_mean)

# Saving observed maize grain Se concentration per cluster (smallest admin boundary).
# saveRDS(cluster_maize, here::here("data", "inter-output", "aggregation", 
  #                               "obs-maize-cluster.RDS"))


### District -----

# Spatially join both dataset

dist_maize <- st_join(geomaize.df, dist_bnd) 
  
dist_maize <- dist_maize %>% 
  st_drop_geometry() %>% filter(!is.na(ADM2_EN)) %>% 
  select(-dist_in_m) %>% 
  group_by(ADM2_EN) %>% 
  summarise(Se_mean = mean(Se_raw), 
            Se_sd = sd(Se_raw), 
            Se_median = median(Se_raw), 
            Se_iqr = IQR(Se_raw), 
            Se_n = n()) %>% 
  arrange(ADM2_EN) 

# dist_maize2 <- maize.df  %>% select(1:25, "EACODE", "Longitude", "Latitude" ) %>% 
#   left_join(., ea_admin %>% 
#                           select(EACODE, ADM2_EN)) %>% distinct() %>% 
#   filter(is.na(dist_in_m)) %>% 
#   group_by(ADM2_EN) %>% 
#   summarise(Se_mean = mean(Se_raw), 
#             Se_sd = sd(Se_raw), 
#             Se_median = median(Se_raw), 
#             Se_iqr = IQR(Se_raw), 
#             Se_n = n()) %>% 
#   arrange(ADM2_EN)

plot(dist_maize$Se_mean, dist_maize2$Se_mean)

#test <- left_join(dist_maize, dist_maize2, by = "ADM2_EN")

# Saving observed maize grain Se concentration per district.
# saveRDS(dist_maize, here::here("data", "inter-output", "aggregation", 
# "obs-maize-district.RDS"))


### Buffers  -----

buff.dist <- na.omit(unique(str_extract(list.files(here::here("data", "inter-output", "boundaries")),
            "[:digit:]{2}")))

geodata.df <- geomaize.df

# Se <- grep("Se", names(geodata.df), value = TRUE)
Se <- "Se_raw"

for(i in 1:length(buff.dist)){

buffer  <- st_read(here::here("data", "inter-output",
                             "boundaries", 
                    paste0("mwi_gps-buffer", buff.dist[i], ".shp"))) %>% 
  rename(survey_cluster1 = "srvy_c1")

maize_buff <- st_join(geodata.df, buffer) 

#test %>% filter(!is.na(srvy_c1)) %>% count(srvy_c1)

maize_buff %>% st_drop_geometry() %>% 
  group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(!!sym(Se)), 
            Se_sd = sd(!!sym(Se)), 
            Se_median = median(!!sym(Se)), 
            Se_iqr = IQR(!!sym(Se)), 
            Se_n = n()) %>% 
  arrange(Se_n) %>% 
  saveRDS(., here::here("data", "inter-output",   "aggregation",  
                        paste0("obs-maize-buffer", 
                        buff.dist[i], ".RDS")))

}


data <- maize_buff %>% st_drop_geometry() %>% 
  group_by(survey_cluster1) %>% 
  summarise(Se_mean = mean(!!sym(Se)), 
            Se_sd = sd(!!sym(Se)), 
            Se_median = median(!!sym(Se)), 
            Se_iqr = IQR(!!sym(Se)), 
            Se_n = n())

data$Se_mean[data$survey_cluster1 == "497"]

## Predicted Se conc.  ----

# Loading the data
# Predicted Se conc. (predicted in 01_maize-model.R)
predmaize.df  <- read.csv(here::here("data", "predicted","Se_raw_OK_maize.csv"))

names(predmaize.df)

predmaize.df$predSe <- exp(predmaize.df$Zhat)

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
#                                 "pred-maize-cluster.RDS"))


### District -----

# Spatially join both dataset

dist_maize <- st_join(geopredmaize.df, dist_bnd) 

Se <- "predSe"

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
# "pred-maize-district.RDS"))


### Buffers  -----


buff.dist <- na.omit(unique(str_extract(list.files(here::here("data", "inter-output", "boundaries")),
                                        "[:digit:]{2}")))

geodata.df <- geopredmaize.df

Se <- grep("Se", names(geodata.df), value = TRUE)

for(i in 1:length(buff.dist)){
  
  buffer  <- st_read(here::here("data", "inter-output",
                                "boundaries", 
                                paste0("mwi_gps-buffer", buff.dist[i], ".shp"))) %>% 
    rename(survey_cluster1 = "srvy_c1")
  
  maize_buff <- st_join(geodata.df, buffer) 
  
  #test %>% filter(!is.na(srvy_c1)) %>% count(srvy_c1)
  
  maize_buff %>% st_drop_geometry() %>% 
    group_by(survey_cluster1) %>% 
    summarise(Se_mean = mean(!!sym(Se)), 
              Se_sd = sd(!!sym(Se)), 
              Se_median = median(!!sym(Se)), 
              Se_iqr = IQR(!!sym(Se)), 
              Se_n = n()) %>% 
    arrange(Se_n) %>% 
    saveRDS(., here::here("data", "inter-output",   "aggregation",  
                          paste0("pred-maize-buffer", 
                                 buff.dist[i], ".RDS")))
  
}


# visual checks

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
