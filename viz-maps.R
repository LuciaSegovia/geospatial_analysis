
# Cleaning the enviroment
rm(list=ls())

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

# National parks
parks  <-  st_read(here::here( "data",
                               "mwi-boundaries", "protected_areas_geo", "protected_areas_geo.shp"))

# Explore the shapefile
head(ea_bnd)

table(sf::st_is_valid(ea_bnd))
table(sf::st_is_valid(parks))

# Removing lakes from boundaries dataset
# Selecting only variables that are interesting 
# EA code, EA area, TA code, district & geometry)
ea_admin <- ea_bnd %>% filter(!grepl("lake", DISTRICT,
                                     ignore.case = TRUE)) %>% 
  select(c(4, 10, 11, 17, 18))



# ea_bnd[which(duplicated(ea_bnd$EACODE)),]  %>% View()
# 
# ea_bnd[which(duplicated(ea_bnd$EACODE)),]  %>%
# tm_shape() +
# tm_polygons()

# Loading data ----

# Linking survey_cluster1 (cluster id) with corresponding admin units 
master <- readRDS(here::here("data", "inter-output", "aggregation", 
                             "master-cluster-admin-level.RDS")) 

# Linking survey_cluster1 (cluster id) with corresponding admin units 
plasma.df <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) 


# Variables and labels ----

# Discrete variables (factors) from number to text
lab_region <- c(`1` = "Northern", `2` = "Central", `3` = "Southern")
lab_reside <- c(`1` = "Urban", `2` = "Rural")
lab_malaria <- c(`1` = "Positive", `2` = "Negative")

# Variables and units (axis - text)
plasma_lab <- expression(paste("Plasma Se conc. (ng  ",  mL^{-1}, ")"))
bmi_lab <- expression(paste("BMI (kg /  ",  m^{2}, ")"))

# Maps! ------

## Base map: Malawi ------

malawi_bnd_lakes <- st_union(ea_bnd) # Aggregate boundaries the whole country (with lakes)
malawi_bnd <- st_union(ea_admin) # Aggregate boundaries the whole country

# The Map:
# Base map (EAs)
base_map <- tm_shape(ea_admin) +    
  tm_polygons(col = "white", 
              border.col = "#666666", border.alpha = 0.3, lwd = 0.2) +
  # Land boundaries
  tm_shape(malawi_bnd) +   
  tm_borders(col = "#666666", alpha = 0.6, lwd = 0.5) +
  # Land/ Lake boundaries
  tm_shape(malawi_bnd_lakes) +
  tm_borders(col = "black", alpha = 0.6, lwd = 0.5) 

#base_map


# Maize data (from 00_cleaning-location.R)
maize.df <- readRDS(here::here("data", "inter-output",  
                               "mwi_maize-se-raw_admin.RDS"))

# Loading Plasma Se with the EACODE allocated ("mwi-plasma-se_admin.RDS")
data.df  <- readRDS(here::here("data", "inter-output", "dhs_se_gps_admin.RDS"))
# Loading Maize Se (pred)
maize_values  <- "predicted-maizeSe" # cleaned  predicted maize Se data
file_name  <- paste0("mwi-", maize_values, "_admin.RDS")
maizedata.df  <- readRDS(here::here("data", "inter-output", file_name))

# Getting the unique EAs where the HHs buffer are co-located
EAselected <- unique(data.df$EACODE)

names(maizedata.df)

x1  <- subset(data.df, !is.na(selenium)) %>% distinct(EACODE)  %>% pull()
x2  <- as.integer(unique(maizedata.df$EACODE))

class(x2)

test_dif  <- setdiff(x1, x2)

sum(is.na(maizedata.df$Latitude))
sum(is.na(data.df$selenium))

# Checking the number of Se values (grain samples) per EA
maizedata.df %>% group_by(EACODE, AREA_KM) %>% dplyr::count()  %>% arrange(desc(n))  %>% 
  dplyr::filter(n > 1) # Checking those larger than one 


# Transforming plasma se into a spatial dataset
#geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
#crs = "EPSG:4326")

# Selecting the Se variable to model
var  <- "logSe"

plot(geodata.df[, var], main = "Se conc. in maize (log(mg/kg))")

# Transforming predicted maize Se into a spatial dataset
maizedata.df   <-  st_as_sf(maizedata.df , coords =c("Longitude", "Latitude"),
                            crs = "EPSG:4326")

# test  <- subset(geodata.df,
#  survey_cluster1 == "309", 
#  select = c(unique_id, DISTRICT, urbanity, EACODE, selenium)) 
# 
# test_centroid  <- st_point_on_surface(ea_bnd[, c(1,18)])


# Checking buffer of EAs w/o co-located values
data  <- subset(ea_bnd, EACODE %in% test_dif, select =c(EACODE, DISTRICT, geometry))
test_centroid  <- st_centroid(data) 
test_buffer2  <- st_buffer(test_centroid, dist = 2000) #2km (2Km urban, 5-10km rural)
test_buffer5  <- st_buffer(test_centroid, dist = 5000) #5km (2Km urban, 5-10km rural)
test_buffer10  <- st_buffer(test_centroid, dist = 10000) #10km (2Km urban, 5-10km rural)

# ea_bnd$fid[ea_bnd$EACODE == "10203024"] 

# ea_bnd$EACODE <- as.character(ea_bnd$EACODE )
# 
# ea_bnd %>% select(geometry, EACODE) %>% 
#   left_join(., data.df %>% filter(admin == "EADIST") %>% 
#     select(admin_id,  maizeSe_mean),
#             by = c("EACODE" = "admin_id")) %>% 
#   tm_shape() +
#   tm_polygons(col = "maizeSe_mean")


# EAs that are to be shown (interested geographies)
EAselected <- unique(maize.df$EACODE)

# For the base map 
base_map %>% 
  # EAs to be shown 
  tm_shape(ea_admin$geometry[ea_admin$EACODE %in% EAselected]) +
  tm_polygons(col ="#314f40", border.col = "black", border.alpha = 0.3)

# Optional: buffers or other features 
#  tm_shape(geodata.df) +
#  tm_borders(col = "steelblue" )



## MAP: Malawi (DISTRICT) ======

dist_bnd  %>% 
  left_join(., data.df %>% filter(admin == "DISTRICT") %>%  
              select(admin_id,  maizeSe_mean),
            by = c("DISTRICT" = "admin_id")) %>% 
  tm_shape() +
  tm_polygons(col = "maizeSe_mean") +
  tm_layout(legend.outside = TRUE)

dist_bnd  %>% 
  left_join(., data.df %>% filter(admin == "DISTRICT") %>%  select(admin_id,  Se_mean),
            by = c("DISTRICT" = "admin_id")) %>% 
  tm_shape() +
  tm_polygons(col = "Se_mean") +
  tm_layout(legend.outside = TRUE)



("Chikwawa", "Blantyre", "Thyolo", "Zomba", "Kasungu", "Mwanza")
grepl("^3", EACODE)

ea_bnd  %>% filter(DISTRICT == "Chikwawa")  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(data$geometry[data$DISTRICT == "Chikwawa"]) +
  tm_polygons(col = "yellow") +
  tm_shape(maizedata.df$geometry[maizedata.df$DISTRICT== "Chikwawa"]) +
  tm_symbols(col = "blue") +
  tm_shape(test_buffer10$geometry[test_buffer10$DISTRICT == "Chikwawa"]) +
  tm_polygons(col = "red", alpha =.5) 


names(test_buffer)
#tm_shape(nso_bound) 

ea_bnd  %>%  filter(DISTRICT == "Chikwawa")  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(test_centroid$geometry[test_centroid$fid=="197"])+
  tm_symbols(size =0.2) +
  tm_shape(test_buffer$geometry[test_buffer$fid=="197"]) +
  tm_polygons(col = "red", alpha =.5) +
  tm_shape(test) +
  tm_symbols(shape = "urbanity", 
             size = "selenium") +
  tm_scale_bar()

# Generating a vector wit the districts (for visualisation)
dist  <- unique(ea_bnd$DISTRICT)

# Removing district 23 (Balaka), 25 (Likoma) bc no value w/o discrepancy & lakes
dist  <- dist[c(1:27) ] # Removing the lakes
dist  <- dist[c(1:22, 24, 26:27) ]  # Remooving 23 (Balaka), 25 (Likoma)
dist  <- dist[c(24, 26:27) ] # Remooving 23 (Balaka), 25 (Likoma)

dist[i]


#data.df   <-  st_as_sf(data.df , coords =c("Longitude", "Latitude"),
# crs = "EPSG:4326")

# Loop that create maps per district w/ maize Se conc. & plasma Se conc.

map  <- list()

for (i in seq_along(dist)) {
  
  test  <- maizedata.df  %>% filter(DISTRICT == dist[i])
  test2  <- geodata.df  %>% filter(DISTRICT == dist[i]) 
  
  map[[i]]  <- ea_bnd  %>% filter(DISTRICT == dist[i])  %>% 
    tm_shape() +
    tm_polygons() +
    tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% test_dif & ea_bnd$DISTRICT == dist[i]]) +
    tm_polygons(col = "red") +
    tm_shape(test) +
    tm_symbols(size = "pred.Se", col = "blue") +
    tm_shape(test2) +
    tm_symbols(size = "selenium", col = "green") +
    tm_layout(main.title = paste0(dist[i], " district"))
  
}

for (i in seq_along(dist)) {
  
  test  <- maizedata.df  %>% filter(DISTRICT == dist[i])
  test2  <- geodata.df  %>% filter(DISTRICT == dist[i]) 
  
  m  <- ea_bnd  %>% filter(DISTRICT == dist[i])  %>% 
    tm_shape() +
    tm_polygons() +
    tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% test_dif & ea_bnd$DISTRICT == dist[i]]) +
    tm_polygons(col = "red") +
    tm_shape(test) +
    tm_symbols(size = "pred.Se", col = "blue") +
    tm_shape(test2) +
    tm_symbols(size = "selenium", col = "green") +
    tm_shape(test_buffer5$geometry[test_buffer5$DISTRICT == dist[i]]) +
    tm_polygons(col = "red", alpha =.5) +
    tm_layout(main.title = paste0(dist[i], " district"))
  
  tmap_save(m, filename=paste0("visuals/map-pred5_", dist[i], ".png"))
  
}

do.call(tmap_save, )

# # Loading the data
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
                                        "Soil_Crop_comparisons", "Malawi",  "Malawi_grain_soil.xlsx"))
names(grain) # checking variables

grain.df   <-  st_as_sf(grain , coords =c("Longitude", "Latitude"),
                        crs = "EPSG:4326")

grain.df   <-  grain.df  %>%
  filter(!is.na(Se_triplequad))

map  <- tm_shape(ea_bnd) +
  tm_polygons() +
  tm_shape(ea_bnd$geometry[ea_bnd$EACODE %in% test_dif]) +
  tm_polygons(col = "red") +
  tm_shape(grain.df) +
  tm_symbols(col = "Crop", 
             size =0.08) +
  tm_layout(legend.show = FALSE) 

tmap_save(map, 
          filename=paste0("visuals/map_GeoNut_se.png"))

data.df  <- data.df  %>% 
  select(selenium, EACODE, survey_cluster1, unique_id, urbanity)  %>% 
  left_join(., maizedata.df, 
            by = c("EACODE"))

# Checking 
nrow(grain)
nrow(grain.df)
sum(!is.na(grain$pH_Wa))
sum(!is.na(grain$Zn))
sum(!is.na(grain$Se_triplequad))



# Checking buffer sizes from district

# District shape file
dist_bnd <- st_read(here::here(
  "..", "PhD_geospatial-modelling", "data", "mwi-boundaries",
  "EN_NSO", "dist_bnd.shp"))


# Generating buffer of EAs w/o co-located values
dist_centroid  <- st_centroid(dist_bnd) 
dist_buffer60  <- st_buffer(dist_centroid, dist = 60000) # 60km - too big (high level of overlapping)
dist_buffer25  <- st_buffer(dist_centroid, dist = 25000) # 25km - good dist coverage
dist_buffer  <- st_buffer(dist_centroid, dist = 26000) # 45km - will cover almost all country


# ea_bnd$fid[ea_bnd$EACODE == "10203024"] 

# dist_bnd  <- st_transform(dist_bnd , crs = 9053)
# dist_bnd  <- st_transform(dist_bnd , crs = 4326)


dist_bnd  %>%  
  tm_shape() +
  tm_polygons(col = "DISTRICT", legend.show = FALSE) +
  tm_shape(dist_buffer) +
  tm_polygons(col = "red", alpha =.5) 

library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addMeasure(primaryLengthUnit = "meters") %>% 
  addMarkers(data = dist_centroid) %>% 
  addPolygons(data = dist_buffer25)


############################

# Comparing predicted maize Se EA mean w/ single values 

data.df  <- readRDS(here::here("data", "inter-output", "maizeSe-mean-predicted.RDS"))

names(data.df)
head(data.df)

maizedata.df$EACODE  <- as.character(maizedata.df$EACODE)

test  <- data.df  %>%  dplyr::filter(admin == "EACODE")  %>% 
  left_join(., maizedata.df, by = c("admin_id" = "EACODE")) 

# Counting maize values per EA
ea  <- test %>% 
  group_by(admin_id)  %>% dplyr::count(maizeSe_mean)  %>% 
  dplyr::filter(n == 1)  %>% arrange(desc(n))  %>% pull(admin_id)


test  %>% filter(admin_id %in% ea)  %>% 
  dplyr::select(maizeSe_mean, Se_triplequad, pred.Se)  %>% View()


### Plasma Se EA data ----

# Map: Plasma visually checking Se conc. w/i District () ----

geoplasma <-  plasma_se %>% left_join(., dist_bnd) %>% st_as_sf()

st_as_sf(coords =c("Longitude", "Latitude"),
         crs = "EPSG:4326")

tm_shape(dist_bnd) +
  tm_borders(col = "black", alpha = 0.6, lwd = 0.5) +
  tm_shape(geoplasma) +
  tm_polygons(col = "Se_mean", border.col = "#666666", border.alpha = 0.3, 
              legend.show = FALSE) 

#  tm_shape(geodata.df) +
#   tm_borders(col = "steelblue" ) 


# Loading the plasma Se conc. dataset (cleaned from 01.cleaning-location.R)
data.df <- readRDS(here::here("data", "inter-output",
                              "raw-maizeSe_plasma-se_ea.RDS" )) #

names(data.df)

data.df <- dplyr::rename(data.df, urbanicity = "urbanity")



# Transforming plasma se into a spatial dataset
geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
                       crs = "EPSG:4326")

geodata.df$logSe <- log(geodata.df$selenium)

plot(geodata.df[, "logSe"])


# Boxplot log(CRP)/(AGP)/(selenium) ~ Malaria
boxplot(log(selenium) ~ Malaria_test_result, data = data.df,
        frame = FALSE)

#data.df <- data.df %>% st_drop_geometry()

plot(log(data.df$selenium) ~ data.df$AGE_IN_YEARS)
plot(log(data.df$selenium) ~ log(data.df$crp))

plot(log(data.df$selenium) ~ log(data.df$agp))

lm(log(data.df$selenium) ~ log(data.df$crp))
lm(log(data.df$selenium) ~ log(data.df$agp))

data.df$EACODE  <- as.character(data.df$EACODE)


sum(is.na(data.df$dist_name))

ggplot(data = data.df # %>% filter(region == 1) 
       ,
       mapping = aes(x = !!sym(var_x), y =!!sym(var_y), colour = EACODE, alpha = 0.5)) +
  geom_point(size = 2) +
  theme_bw() +
  facet_wrap(~DISTRICT) +
  labs(
    x = var_x, 
    y = var_y) + 
  theme(legend.position = "none")


var_x <- "AGE_IN_YEARS"
var_y <- "selenium"
var_col <- "EACODE"

# Visualising data per region ----

ggplot(data = data.df,
       mapping = aes(x = !!sym(var_x), y =!!sym(var_y), colour = !!sym(var_col))) +
  geom_point(size = 2, alpha = 0.5) +
  theme_bw() +
  facet_wrap(~region, labeller = as_labeller(lab_region)) +
  labs(x = "Age (years)", 
       y = expression(paste("Plasma Se conc. (ng ",  mL^{-1}, ")"))) + 
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        legend.position = "none")


# Boxplots ----

## Age per region and residency ----
## Continous per malaria test and residency ----
#var_x <- "agp"
#x_label <- "AGP(mg/l)"
#var_x <- "crp"
#x_label <- "log c-reactive protein (mg/l)"
#var_x <- "AGE_IN_YEARS"
#x_label <- "Age (years)"
var_x <- "selenium"
x_label <- plasma_lab
#var_col <- "Malaria_test_result"
#col_lab <- lab_malaria
var_col <- "BMI_cat"

value_median <- median(pull(plasma.df[,var_x]), na.rm = TRUE)
value_median <- log(median(pull(plasma.df[,var_x]), na.rm = TRUE))
#value_median <- median(plasma.df$AGE_IN_YEARS)
#value_median <- median(plasma.df$selenium, na.rm = TRUE)

plasma.df %>% 
  mutate(BMI_cat = as.factor(case_when(
    BMI<18.5 ~ "low",
    BMI>18.5 & BMI <24.5 ~ "healthy",
    BMI>24.5 ~ "high")),
    BMI_cat = forcats::fct_relevel(BMI_cat, "low", "healthy", "high")) %>% 
  filter(!is.na(BMI_cat)) %>% 
 # mutate(region = forcats::fct_relevel(region,"3", "2", "1")) %>% 
 # ggplot(aes(x = log(!!sym(var_x)), weight=wt, # w/ survey weights
  ggplot(aes(x = !!sym(var_x), weight=wt, # w/ survey weights
             #region, 
             !!sym(var_col),
             colour=!!sym(var_col))) +
  geom_vline(xintercept = value_median, col = "lightgrey", size = 1) +
 # geom_vline(xintercept = log(5), col = "linewidth", size = 1) +
  #geom_boxplot() + 
  geom_violin() + 
 # scale_colour_discrete(name = "", label = col_lab) +
    coord_flip() +
  scale_y_discrete(label = lab_region) +
  theme_classic() +
  labs(y = "",
       x = x_label) +
  theme(legend.position = "top") +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10))

## Plasma Se per EA for each district ----

var_x <- "EACODE"
var_y <- "selenium" 
var_col <- "urbanicity"

n_breaks <- unique(data.df[, var_col])
show_col(hue_pal()(3))

col_break <- c("1" = "#00BFC4", "2" = "#F8766D")

unique(data.df$DISTRICT)
unique(data.df$DISTRICT[data.df$urbanity=="1"])

plot_dist <- list()

for(i in 1:length(unique(data.df$DISTRICT))){
  
  plot_dist[[i]] <- ggplot(data = data.df  %>% filter(DISTRICT %in%  unique(data.df$DISTRICT)[i]) # for viewing by region
                           ,
                           mapping = aes(x = !!sym(var_x), y =!!sym(var_y),
                                         colour = !!sym(var_col))) +
    geom_boxplot() +
    theme_light() +
    scale_colour_manual(values =  col_break) +
    labs(title =  unique(data.df$DISTRICT)[i],
         x = var_x, 
         y = var_y) # + 
  #theme(legend.position = "none")
  
}

print(plot_dist[[4]])


# Boxplot: Continuous by two categorical variable by region
var_x <- "survey_cluster1"
var_y <- "selenium" 
var_col <- "urbanity"

data.df$survey_cluster1 <- as.character(data.df$survey_cluster1)

n_breaks <- unique(data.df[, var_col])
show_col(hue_pal()(3))

# Custom legend colour & labels
col_break <- c("1" = "#00BFC4", "2" = "#F8766D")
#col_labels <- c("1" = "Urban", "2" = "Rural")

# Custom X-axis labels 
# col_labels <- c("Urban", "Rural")


ggplot( data = data.df, 
       mapping = aes(x = !!sym(var_x), y =!!sym(var_y),
                     colour = !!sym(var_col))) +
  geom_boxplot() +
  theme_classic() +
  scale_colour_manual(values =  col_break,
                      # breaks = col_break,
                      labels = lab_reside) +
  facet_wrap(~region, labeller = as_labeller(lab_region),
             scales = "free_x") +
  # scale_x_discrete(label = labels) +
  labs(
    y = expression(paste("Plasma Se conc. (ng  ",  mL^{-1}, ")"))) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10, angle =30))

# Points ----- 

## Plasma Se and age by region and malaria test ----

# var_x <- "AGE_IN_YEARS"
var_x <- "BMI"

plasma.df %>% 
  mutate(BMI_cat= case_when(
          BMI<18.5 ~ "low",
          BMI>18.5 & BMI <24.5 ~ "healthy",
          BMI>24.5 ~ "high")) %>% 
  #ggplot(aes(AGE_IN_YEARS, selenium, colour =Malaria_test_result)) + 
  ggplot(aes(!!sym(var_x), log(selenium), colour = BMI_cat)) + 
  geom_point() +
  geom_smooth(method = "auto") +
 # geom_hline(yintercept = 84.9, colour = "red") +
  theme_minimal() +
   theme(legend.position = "bottom") # +
#  scale_colour_manual(values = my_colour) # +
 #  facet_wrap(~Malaria_test_result) 
#  facet_wrap(~region, labeller = as_labeller(lab_region)) 

# Map: Plasma visually checking buffer areas and EAs (00_cleaning-location.R) ----

geodata.df <- data.df %>% dplyr::select(survey_cluster1, buffer) %>% 
  distinct() %>% 
  dplyr::rename(geometry = "buffer") %>% st_sf(., crs = "EPSG:4326")


# Tested green (#2D8733)
base_map +
  tm_shape(ea_admin$geometry[ea_admin$EACODE %in% EAselected]) +
  tm_polygons(col ="firebrick4", border.col = "black", border.alpha = 0.3) +
  tm_shape(geodata.df) +
  tm_borders(col = "steelblue" )

## Cluster maize aggregation MAP -----
# Loading the maize data
# Getting district files
file <- grep("cluster", list.files(here::here("data", "inter-output", "aggregation")), 
             value = TRUE)

# Choosing the file to map
i = 3

# Getting the data, combining with boundaries
maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               file[i])) %>%   # Aggregated Se conc
  left_join(., master) %>%              # Admin level info
  left_join(., ea_bnd %>% select(EACODE, geometry))  # Boundaries for the admin
st_sf(., crs = "EPSG:4326")  # Boundaries for the admin

maize.df$log_Se <- log(maize.df$Se_mean)

# For the log-trasformed to keep the scale consistent we need to reverse it
# using "-palette.name". (e.g., palette = "-YlOBr")
base_map +
  tm_shape(maize.df) +
  tm_polygons(col = "Se_mean", palette = "YlOrBr") +
  #  tm_polygons(col = "log_Se", palette = "-YlOrBr") +
  tm_layout(legend.outside = TRUE, legend.text.size = 1.5)


## District maize aggregation MAP -----

# Loading the maize data
# Getting district files
file <- grep("distric", list.files(here::here("data", "inter-output", 
                                              "aggregation")),  value = TRUE)
# Choosing the file to map
i = 2

# Getting the data, combining with boundaries
maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               file[i])) %>%          # Aggregated Se conc
  left_join(., master %>% select(survey_cluster1, ADM2_EN))  %>%  # Admin level info
  left_join(., dist_bnd %>% select(ADM2_EN, geometry))  %>%  # Boundaries for the admin
  st_sf(., crs = "EPSG:4326") # Transform into spatial obj

base_map +
  tm_shape(maize.df) +
  tm_polygons(col = "Se_sd", palette = "YlOrBr") +
  #  tm_polygons(col = "log_Se", palette = "-YlOrBr") +
  # tm_layout(legend.outside = TRUE, legend.text.size = 1.5) +
  # Legend outside and hist 
  tm_legend(legend.outside=T, legend.outside.position="right", legend.text.size = 1.5)

# Map: Plasma visually checking cluster EAs are w/i District (00_cleaning-location.R) ----

tm_shape(dist_bnd) +
  tm_borders(col = "black", alpha = 0.6, lwd = 0.5) +
  tm_shape(ea_admin$geometry[ea_admin$EACODE %in% unique(geodata_ea$EACODE)]) +
  tm_polygons(col ="firebrick4", border.col = "#666666", border.alpha = 0.3) +
  tm_shape(geodata.df) +
  tm_borders(col = "steelblue" )


# Point: Plasma and maize at EA level (What's the cluster?) ----

## Loading data

# Plasma dataset with admin aggregation unit (from 00_cleaning-location.R)
plasma.df <- readRDS(here::here("data", "inter-output", 
                                paste0("dhs_se_gps_admin.RDS")))

# Selecting variables of interest
plasma_ea <- plasma.df %>%  select(selenium, wealth_quintile, urbanity, region, 
                                   survey_cluster1, unique_id, EACODE)

# Getting colours for the clusters in the different regions 
my_colour <-   c(paletteer::paletteer_c("ggthemes::Green", 35),
                 paletteer_c("ggthemes::Classic Red", 34),
                 paletteer_c("ggthemes::Classic Blue", 33))


names(my_colour) <- plasma.df %>% select(survey_cluster1, region) %>% 
  distinct() %>% #filter(region == "1") %>% 
  arrange(region) %>% pull(survey_cluster1)

pred_ea %>% 
  right_join(., plasma_ea) %>% mutate_at("survey_cluster1", as.character) %>% 
  #  filter(region == "1") %>% 
  ggplot(aes(log(predSe), selenium, colour =survey_cluster1)) + 
  geom_point() +
  geom_hline(yintercept = 84.9, colour = "red") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_manual(values = my_colour) # +
# facet_wrap(~region, labeller = as_labeller(lab_region)) 



# Ridges: Maize distribution per cluster ----

unique(plasma_ea$unique_id)[1:10]


pred_ea %>% 
  right_join(., plasma_ea) %>% 
  dplyr::filter(survey_cluster1 %in%  unique(plasma_ea$survey_cluster1)[1]) %>% 
  ggplot(aes(x = predSe, y = unique_id)) +
  geom_density_ridges() 

pred_ea %>% 
  right_join(., plasma_ea) %>%  mutate_at("survey_cluster1", as.character) %>% 
  #  dplyr::filter(survey_cluster1 %in%  unique(plasma_ea$survey_cluster1)[1]) %>% 
  ggplot(aes(x = predSe, y = survey_cluster1, fill = stat(quantiles))) +
  geom_density_ridges_gradient() +
  scale_fill_viridis_c(name = "Maize Se", option = "C") +
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal() +
  facet_wrap(~region, labeller = as_labeller(c(`1` = "Northern", 
                                               `2` = "Central", 
                                               `3` = "Southern")),
             scales = "free")  


# Ridges: Plasma Se per region and residency----

var_x <- "selenium"
x_label <- plasma_lab
  
Malawi_WRA %>% 
  mutate(region = forcats::fct_relevel(region,"3", "2", "1")) %>% 
  ggplot(aes(x = !!sym(var_x), weight=wt,  region, fill=urbanity, colour = urbanity)) +
  #geom_boxplot() + 
  ggridges::geom_density_ridges(alpha = 0.5)  +
  scale_fill_discrete(name = "", label = lab_reside) +
  scale_colour_discrete( guide = "none") +
  #  geom_violin() + 
  #  geom_vline(xintercept = median(Malawi_WRA$AGE_IN_YEARS), col = "lightyellow", size = 1) +
  #  coord_flip() +
  scale_y_discrete(label = lab_region) +
  theme_classic() +
  labs(y = "",
       x = x_label) +
  theme(legend.position = "top") # +
#  ggridges::theme_ridges(center = TRUE)

# Distribution: Maize Se distribution  ----



