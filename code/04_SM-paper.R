###############################################################################
#   
#   
#   This script contains all the script to replicate Suppl. Mat.
#                   used in the manuscript:
#     The optimal way of aggregating geo-referenced Se concentration
#     for nutritional assessment purposes
#   
#   
###########

# Loading libraries
library(dplyr)
library(tidyr)
library(sp)
library(sf) # for reading in and writting shapefiles
library(raster) # raster manipulation
library(tmap)  #spatial data manipulation and visualisation
library(sfheaders) # 'deconstructing’ and ‘reconstructing’ sf objects
source("functions/CEPHaStat_3.R")

# Loading maize & other grain final dataset (all combined) (from 00_cleaning-maize.R)
data.df <- readRDS(here::here("data", "inter-output", "mwi-grain-se_raw.RDS"))


## SM - Fig.1a -----------------

# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
                        crs = "EPSG:4326")

# Checking sample coverage location - Spatial pattern?
plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "blue",  
     axes = TRUE)
plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & is.na(geodata.df$Crop)], col = "green", add = TRUE)
plot(geodata.df$geometry[is.na(geodata.df$Se_grain) & geodata.df$Crop == "Maize"], col = "red", add = TRUE)
plot(geodata.df$geometry[!is.na(geodata.df$Se_grain) & geodata.df$Crop != "Maize"], col = "yellow", add = TRUE )

locsv<-c(-16.2,-16.5,-16.8, -17.1)
locsv<-c(-9.5,-10,-10.5, -11)
cols <- c("blue","green","red","yellow")
points(35,locsv[1],pch=16, col=cols[1])
points(35,locsv[2],pch=16, col=cols[2])
points(35,locsv[3],pch=16, col=cols[3])
points(35,locsv[4],pch=16, col=cols[4])

text(35.2,locsv[1],"Maize grain (GeoNutrition)",pos=4)
text(35.2,locsv[2],"Maize grain (Chilimba)",pos=4)
text(35.2,locsv[3],"Maize grain (<LOD)",pos=4)
text(35.2,locsv[4],"Other grains",pos=4)


## SM- Fig.1b -----------------

data.df %>% filter(!is.na(Se_raw)) %>%  count(Crop)
data.df$Crop_lab <- data.df$Crop

data.df$Crop_lab[data.df$Crop == "Maize"] <- "Maize\n(n=1689)"
data.df$Crop_lab[data.df$Crop == "Finger millet"] <- "Finger millet\n(n=1)"
data.df$Crop_lab[data.df$Crop == "Pearl millet"] <- "Pearl millet\n(n=32)"
data.df$Crop_lab[data.df$Crop == "Rice"] <- "Rice\n(n=53)"
data.df$Crop_lab[data.df$Crop == "Sorghum"] <- "Sorghum\n(n=117)"

boxplot(Se_raw ~ Crop_lab, data.df, xlab ="", 
        ylab = expression(paste("Se (mcg  ",  Kg^{-1}, ")")))

## SM - Fig. 2 -----
# Both 2a and 2b are in 01_maize-model.R

## SM - Fig.3 -----------------

data.df <- read.csv(here::here("data", "OK", "2024-05-03Se_raw_OK_expmaize.csv"))

# Generating spatial dataset
geodata.df  <- st_as_sf(data.df , coords =c("Longitude", "Latitude"),
                        crs = "EPSG:4326")

plot(geodata.df[,  "Zhat_exp"])
plot(geodata.df[,  "Zhat"])

# Converting into raster
dfr <- rasterFromXYZ(data.df)  #Convert first two
#names(dfr) <- c("Mean", "SD") # Change names
         
plot(dfr, "Zhat_exp")           

element <- "kv"
element <- "Zhat"
title <-  "Kriging Variance"
title <-  "Predicted Se (log-transformed)"
quantiles<-as.character(cut(data.df[,element],quantile(data.df[,element],
                                                       c(0,0.25,0.5,0.75,1)),include.lowest=T,
                            label=c("blue","green","yellow","red")))	

# Classified post-plot

options(scipen=5)
dev.new()
plot(data.df[,"Longitude"],data.df[,"Latitude"], # Lon & Lat variables
     pch=16,col=quantiles,asp=1, main = title,
     xlab="Longitude",ylab="Latitude",cex=0.5)
text(38,-9.5,"(a)",pos=4) # Plot label

locsv<-c(-16.2,-16.5,-16.8, -17.1)
cols <- c("blue","green","yellow","red")
points(30,locsv[1],pch=16, col=cols[1])
points(30,locsv[2],pch=16, col=cols[2])
points(30,locsv[3],pch=16, col=cols[3])
points(30,locsv[4],pch=16, col=cols[4])

text(30.2,locsv[1],"Lowest Quantile (0-25%)",pos=4)
text(30.2,locsv[2],"Mid-lower Quantile (25-50%)",pos=4)
text(30.2,locsv[3],"Mid-higher Quantile (50-75%)",pos=4)
text(30.2,locsv[4],"Highest Quantile (75-100%)",pos=4)


## SM - Fig. 4 -----

# Linking survey_cluster1 (cluster id) with corresponding admin units 
data <- readRDS(here::here("data", "inter-output", "model", 
                             "plasma-obs-maize-buffer25.RDS")) %>% 
  left_join(., readRDS(here::here("data", "inter-output", "aggregation", 
                               "pred-maize-buffer25_v2.0.0.RDS")), 
            by = "survey_cluster1")  


plot(log(data$Se_median.x), log(data$Se_median.y))


# SM - Fig.5 ---- 

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
