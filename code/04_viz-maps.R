
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
  dplyr::select(c(4, 10, 11, 17, 18))



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

# Colour choices ----

# 1) Plasma
#"firebrick4"

# Urban and rural
col_break <- c("1" = "#00BFC4", "2" = "#F8766D")

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
var_x <- "AGE_IN_YEARS"
x_label <- "Age (years)"
#var_x <- "selenium"
#x_label <- plasma_lab
#var_col <- "Malaria_test_result"
#col_lab <- lab_malaria
var_col <- "BMI_cat"

value_median <- median(pull(plasma.df[,var_x]), na.rm = TRUE)
#value_median <- log(median(pull(plasma.df[,var_x]), na.rm = TRUE))
#value_median <- median(plasma.df$AGE_IN_YEARS)
#value_median <- median(plasma.df$selenium, na.rm = TRUE)

plasma.df  %>% 
  mutate(BMI_cat = as.factor(case_when(
    BMI<18.5 ~ "low",
    BMI>18.5 & BMI <24.5 ~ "normal",
    BMI>24.5 ~ "high")),
    BMI_cat = forcats::fct_relevel(BMI_cat, "low", "normal", "high")) %>% 
 # mutate(BMI_cat = as.factor(case_when(
 #   BMI<30 ~ "non-obese",
 #   BMI>=30 ~ "obese")),
 #   BMI_cat = forcats::fct_relevel(BMI_cat, "non-obese", "obese")) %>% 
  filter(!is.na(BMI_cat)) %>%  # count(BMI_cat)
 # mutate(region = forcats::fct_relevel(region,"3", "2", "1")) %>% 
 # ggplot(aes(x = log(!!sym(var_x)), weight=wt, # w/ survey weights
  ggplot(aes(x = !!sym(var_x), weight=wt, # w/ survey weights
             #region, 
             !!sym(var_col),
             colour=!!sym(var_col))) +
  geom_vline(xintercept = value_median, col = "lightgrey", size = 1) +
 # geom_vline(xintercept = log(5), col = "linewidth", size = 1) +
  geom_boxplot() + 
#  geom_violin() + 
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

# Boxplot: Age per region and residency----
#  fig.cap = "BMI (kg/m^2) of women by residency and region in Malawi. The light grey lines represent the healthy BMI range"
var_x <- "BMI"
x_label <-  expression(paste("BMI (kg /  ",  m^{2}, ")"))

plasma.df %>% 
  mutate(region = forcats::fct_relevel(region,"3", "2", "1")) %>% 
  ggplot(aes(x = !!sym(var_x), weight=wt, region, colour=urbanity)) +
  geom_vline(xintercept = 18.5, col = "lightgrey", size = 1) +
  geom_vline(xintercept = 24.5, col = "lightgrey", size = 1) +
  geom_boxplot() + 
  scale_colour_discrete(name = "", label = lab_reside) +
  #  geom_violin() + 
  coord_flip() +
  scale_y_discrete(label = lab_region) +
  theme_classic() +
  labs(y = "",
       x = x_label) +
  theme(legend.position = "top")

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
 # ggplot(aes(!!sym(var_x), log(selenium), colour = BMI_cat)) + 
  ggplot(aes(AGE_IN_YEARS, !!sym(var_x), colour = BMI_cat)) + 
  geom_point() +
 # geom_smooth(method = "auto") +
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
  left_join(., ea_bnd %>% dplyr::select(EACODE, geometry)) %>%  # Boundaries for the admin
st_sf(., crs = "EPSG:4326")  # Boundaries for the admin

maize.df$log_Se <- log(maize.df$Se_mean)

# For the log-transformed to keep the scale consistent we need to reverse it
# using "-palette.name". (e.g., palette = "-YlOBr")
base_map +
  tm_shape(maize.df) +
 # tm_polygons(fill = "Se_mean", fill.scale = tm_scale("YlOrBr")) +
    tm_polygons(fill = "log_Se", fill.scale = tm_scale("-YlOrBr")) +
  tm_layout(legend.outside = TRUE, legend.text.size = 1.5)

# For the log-transformed to keep the scale consistent we need to reverse it
# using "-palette.name". (e.g., palette = "-YlOBr")
mean[i] <- base_map +
  tm_shape(maize.df) +
  tm_polygons(fill = "Se_mean", 
              fill.scale = tm_scale_continuous(values="brewer.yl_or_br"), 
              col = "grey", col_alpha = 0.2) #+
 # tm_layout(legend.outside = TRUE, legend.text.size = 1.5)


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

# Figure 2: Tests & trials  ----

# Figure 2 - Map of EA group & detail ---- 
## Data: Maps for Maize aggregation  -----

# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
# Cluster's EA info
# cluster.df <- readRDS(here::here("data", "inter-output", 
#                                  "dhs_se_gps_admin.RDS"))  %>% 
#   distinct(survey_cluster1, EACODE)
# 
# ea_admin <- st_read(here::here( "data", "inter-output", 
#                                 "boundaries", "mwi_admbnda_adm4_nso.shp"))  %>% 
#   left_join(., cluster.df)
# 
# ea_admin$region <- as.factor(ea_admin$region)


file <- grep("plasma.*v2.0.0", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

# EA group data
plasma_se <- readRDS(here::here("data", "inter-output", "model",
                                file[9])) 
# Adding the spatial data
data.df <- plasma_se %>% select(survey_cluster1, Se_median, URBAN_RURA) %>% 
  left_join(., ea_admin) %>% st_as_sf()

anti_join(data.df %>% st_drop_geometry(), ea_admin %>% st_drop_geometry())

eas_group <- ea_admin %>% st_drop_geometry() %>% filter(!is.na(survey_cluster1)) %>% 
  distinct() %>% left_join(ea_admin %>% select(survey_cluster1, geometry)) %>%
  filter(!is.na(ADM1_EN)) %>%   st_as_sf()



tm_shape(ea_admin) +
  tm_polygons() +
  tm_shape(data.df) +
  tm_polygons(fill = "Se_median") +
  tm_shape(ea_admin) +
  tm_borders(fill = "survey_cluster1",  fill.legend = tm_legend_hide())

#From 00_celaning-dhs.R
class(ea_admin$survey_cluster1) 

ea_admin$survey_cluster1 <- as.character(ea_admin$survey_cluster1)
#GPS$survey_cluster1 <- as.character(GPS$survey_cluster1)

# https://carto.com/carto-colors/
"#66C5CC","#F6CF71","#F89C74","#DCB0F2","#87C55F","#9EB9F3","#FE88B1","#C9DB74",
"#8BE0A4","#B497E7","#D3B484","#B3B3B3"


#tmap_mode("view")

center <- st_coordinates(st_centroid(lilongwe_bbox))

# Plot with zoom

tm_shape(ea_admin) +
  tm_borders(col_alpha = 0.1) +
  tm_shape(lilongwe_bbox) +
  tm_borders(col = "red", lwd = 2) +
  tm_view(set_view = c(center[1]+10, center[2]+10, zoom = 9))  # adjust zoom level as needed


# Figure 2 - MAP detail ---- 

GPS <- GPS %>% mutate(across(URBAN_RURA, ~if_else(URBAN_RURA == "U", "Urban", "Rural", 
                                                  missing = NA)))

map_lilongwe <- 
  tm_shape(dist_bnd, bbox = st_bbox(li)) +
  tm_borders() +
  # tm_shape(lilongwe_bbox) +
  #  tm_borders(col = "red", lwd = 2)
  #  tm_shape(ea_admin %>% filter(grepl("Lilongwe", ADM2_EN))) +
  #  tm_polygons(col_alpha = 0.1, col = "white") +
  tm_shape(ea_admin %>% filter(grepl("Lilongwe", ADM2_EN), 
                               !is.na(survey_cluster1)))+
  tm_polygons(fill = "ADM1_EN", 
              fill.scale = tm_scale_categorical(values = c( "#9EB9F3")),
              col_alpha = 0.1, 
              fill.legend = tm_legend(show = FALSE)) +
  tm_shape(GPS %>% filter(grepl("Lilongwe", ADM2_EN))) +
  tm_borders(col = "URBAN_RURA", 
             #  col.scale = tm_scale_categorical(values = c("#FCB97D", "#FE88B1" )),
             #     col.scale = tm_scale_categorical(values = c("#FCB97D", "#8BE0A4" )),
             col.scale = tm_scale_categorical(values = c("#FCB97D", "#f39eb9" )),
             lwd =2.8, 
             col.legend = tm_legend(title = "",   orientation = "landscape",
                                    position = tm_pos_in("right", "bottom"), 
                                    item.height = 1, item.width = 3,
                                    lwd = 2)) +
  tm_scalebar(position = c("left", "bottom"))

map_lilongwe
print(map_malawi, vp = grid::viewport(0.6, 0.18, width = 0.2, height = 0.5))



tmap_mode("plot")

# Main map: zoomed-in Lilongwe
main_map <- tm_shape(dist_bnd, bbox = st_bbox(li)) +
  tm_borders() +
  tm_shape(li) +
  tm_borders(col = "blue", lwd = 2) +
  tm_layout(title = "Lilongwe District (Zoomed)", frame = FALSE)

# Inset map: full Malawi with red box showing Lilongwe's location
inset_map <- tm_shape(dist_bnd) +
  tm_borders() +
  tm_shape(lilongwe_bbox) +
  tm_borders(col = "red", lwd = 2)

# Combine both with inset positioned in bottom right
main_map +
  tm_inset(inset_map,
           position = c("right", "bottom"),  # or use numeric position c(xmin, ymin, xmax, ymax)
           width = 0.3,  # relative width
           height = 0.3
  )


# Figure 2 -test ----
# https://upgo.lab.mcgill.ca/2019/12/13/making-beautiful-maps/

main <- ea_admin %>%
  mutate(visualise = ifelse(is.na(survey_cluster1), NA, ADM1_EN)) %>% 
  ggplot() +
  geom_sf(
    aes(fill = visualise), 
    lwd =  0.000001,
    colour = "white") +
  scale_fill_manual(
    values = c("#9DBF9E", "#FCB97D", "#A84268"), 
    na.value = "grey80") +
  # Set a completely blank theme, to get rid of all background and axis elements
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(1, 0),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position.inside = c(0.90, 0.75)
  )

lilongwe <-  ea_admin %>% filter(grepl("Lilongwe", ADM2_EN))

## Zoom over lilongwe ----
main +
  coord_sf(
    xlim = sf::st_bbox(lilongwe)[c(1,3)],
    ylim = sf::st_bbox(lilongwe)[c(2,4)],
    expand = FALSE
  ) +
  theme(legend.position = "none")




main <-  
  # mutate(visualise = ifelse(is.na(survey_cluster1), NA, ADM1_EN)) %>% 
  ggplot() +
  geom_sf(dist_bnd) +
  geom_sf(eas_group, 
          aes(fill = ADM1_EN)) +
  scale_fill_manual(
    values = c("#9DBF9E", "#FCB97D", "#A84268"), 
    na.value = "grey80") +
  # Set a completely blank theme, to get rid of all background and axis elements
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(1, 0),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0.90, 0.75)
  ) 

unique(ea_admin$ADM2_EN)

tm_shape(ea_admin)


tm_shape(ea_admin %>% filter(region == "3")) +
  tm_borders() +
  tm_shape(ea_admin %>% filter(ADM2_EN == "Mangochi" & !is.na(survey_cluster1) & is.na(ADM1_EN))) +
  tm_polygons(fill = "red")

# SM - (old) Figure 5: Aggregations mean & sd ----
# This may be a good "interactive project" for the docu.
# For mapping
source("viz_base-map.R")

# Linking survey_cluster1 (cluster id) with corresponding admin units 
master <- readRDS(here::here("data", "inter-output", "aggregation", 
                             "master-cluster-admin-level.RDS")) 

## Data ----
(file <- grep("pred-maize.*._v2", list.files(here::here("data", "inter-output", "aggregation")), 
              value = TRUE))

mean <- list()
sd <- list()

# For storing into pdf

pdf("visuals/aggregation-mean-and-sd.pdf", onefile = TRUE)

i =1
# Looping over all model resutls
for(i in 1:10){
  print(i)
  # Getting the data, combining with boundaries
  maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                                 file[i])) %>% left_join(., master)
  
  title <- gsub("_v2.0.0.RDS", " ", file[i])
  
  
  if(i<9){
    
    (buff.dist <- na.omit(unique(stringr::str_extract(list.files(here::here("data", 
                                                                            "inter-output", "boundaries", "buffer")),
                                                      "[:digit:]{2}"))))
    
    buffer  <- st_read(here::here("data", "inter-output",
                                  "boundaries", "buffer",
                                  paste0("mwi_buffer", buff.dist[i], ".shp"))) %>% 
      dplyr::rename(survey_cluster1 = "srvy_c1")
    
    graph.df <-   maize.df %>% 
      left_join(., buffer) %>% distinct() %>%  # Boundaries for the admin
      st_sf(., crs = "EPSG:4326")  # Boundaries for the admin
    
  } else{
    
    if(i==9){
      
      graph.df <-  maize.df %>% 
        left_join(., ea_bnd %>% dplyr::select(EACODE, geometry)) %>%  # Boundaries for the admin
        st_sf(., crs = "EPSG:4326")  # Boundaries for the admin
      
    }else{
      
      graph.df <-    maize.df %>% 
        left_join(., dist_bnd %>% dplyr::select(ADM2_PCODE, geometry)) %>%  # Boundaries for the admin
        st_sf(., crs = "EPSG:4326")  # Boundaries for the admin
    }
    
  }
  
  # Mean & SD maize Se conc. 
  i =1
  
  mean[[i]] <- base_map +
    tm_shape(graph.df) +
    tm_polygons(fill = "Se_mean", 
                fill.scale = tm_scale_continuous(values="brewer.yl_or_br"), 
                col = "grey", col_alpha = 0.2)  +
    tm_title_out(title)
  
  sd[[i]] <- base_map +
    tm_shape(graph.df) +
    tm_polygons(fill = "Se_sd", 
                fill.scale = tm_scale_continuous(values="brewer.yl_or_br"), 
                col = "grey", col_alpha = 0.2) +
    tm_title_out(" ")
  
  # Arrange plots
  (tm <- tmap_arrange(mean[[i]], sd[[i]]))
  
  tmap_save(tm, paste0("visuals/", title[i], "maps.png"),
            width = 1000, height = 750, dpi = 300)
  
}


dev.off()

