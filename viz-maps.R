
# Cleaning the enviroment
rm(list=ls())

# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(scales) # visualisation - colours
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation


###########################################################################################


#    Shapefiles


##########################################################################################

# Loading Shapefilees 

# Admin Boundaries for Malawi 

# EAs
ea_bnd  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "EN_NSO" , "eas_bnd.shp"))

dist_bnd <- st_read(here::here("..", "PhD_geospatial-modelling",   #folder for storing (all) shapefiles 
           "data", "mwi-boundaries",   "EN_NSO", "dist_bnd.shp"))

# ea_bnd[which(duplicated(ea_bnd$EACODE)),]  %>% View()
# 
# ea_bnd[which(duplicated(ea_bnd$EACODE)),]  %>%
# tm_shape() +
# tm_polygons()

######################################################
# Loading Plasma Se with the EACODE allocated
data.df  <- readRDS(here::here("data", "inter-output","mwi-plasma-se_admin.RDS"))
# Loading Maize Se (pred)
maize_values  <- "predicted-maizeSe" # cleaned  predicted maize Se data
file_name  <- paste0("mwi-", maize_values, "_admin.RDS")
maizedata.df  <- readRDS(here::here("data", "inter-output", file_name))

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
geodata.df <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
 crs = "EPSG:4326")

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

## MAP: Malawi (EA) ======

ea_bnd$EACODE <- as.character(ea_bnd$EACODE )

ea_bnd %>% select(geometry, EACODE) %>% 
  left_join(., data.df %>% filter(admin == "EADIST") %>%  select(admin_id,  maizeSe_mean),
            by = c("EACODE" = "admin_id")) %>% 
  tm_shape() +
  tm_polygons(col = "maizeSe_mean")

## MAP: Malawi (DISTRICT) ======

dist_bnd  %>% 
  left_join(., data.df %>% filter(admin == "DISTRICT") %>%  select(admin_id,  maizeSe_mean),
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
 
 
 ### Visualising EA data ----
 
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
   facet_wrap(~region, labeller = as_labeller(c(`1` = "Northern", 
                                     `2` = "Central", 
                                     `3` = "Southern"))) +
  labs(x = "Age (years)", 
       y = expression(paste("Plasma Se conc. (ng ",  mL^{-1}, ")"))) + 
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        legend.position = "none")
 
# Boxplots per EA for each district ----
 
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
 
 
 var_x <- "urbanicity"
 var_y <- "selenium" 
 var_col <- "urbanicity"
 
 
 n_breaks <- unique(data.df[, var_col])
 show_col(hue_pal()(3))
 
 col_break <- c("1" = "#00BFC4", "2" = "#F8766D")
 
 # Custom X-axis labels 
 labels <- c("Urban", "Rural")
 
 
 ggplot(data = data.df 
        ,
        mapping = aes(x = !!sym(var_col), y =!!sym(var_y),
                      colour = !!sym(var_col))) +
   geom_boxplot() +
   theme_classic() +
   scale_colour_manual(values =  col_break) +
   facet_wrap(~region, labeller = as_labeller(c(`1` = "Northern", 
                                                `2` = "Central", 
                                                `3` = "Southern"))) +
   scale_x_discrete(label = labels) +
   labs(
        y = expression(paste("Plasma Se conc. (ng ",  mL^{-1}, ")"))) + 
   theme(strip.text = element_text(size = 12),
         axis.text.y = element_text(size = 12))

 