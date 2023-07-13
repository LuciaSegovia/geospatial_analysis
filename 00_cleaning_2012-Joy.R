

library(dplyr)
library(sp)
library(sf) # for reading in and writting shapefiles
library(tmap) #visualising maps

# Loading data 
crop  <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =6, skip =2)

# Tidying table
# Getting variables names for the  1:8 variables
names(crop)[1:8]  <-  crop[7, c(1:8)]
names(crop)
# Removing empty/unnecessary rows
crop  <- crop[8:nrow(crop),]

# Select variables of interest
crop[, c("Sample_number", "X_coord", "Y_coord", "Crop", "Se")]

crop  <- crop  %>% select( c("Sample_number", "X_coord", "Y_coord", "Crop",
"FAO_soil_classification",  "Collection_notes",  "Se"))  %>% 
dplyr::filter(grepl("maize", Crop, ignore.case = TRUE) & is.na(Collection_notes))  %>%  # Selecting only maize grain, excluding flour
rename(FAO_soil ="FAO_soil_classification") # renaming

boxplot(Se ~ FAO_soil, crop)

# Loading data 
soil <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =5, skip =2)
names(soil)

# Select variables of interest
soil <- soil  %>% select(c("Sample_number", "pH", "FAO soil classifcation"))  %>% 
                                    filter(!is.na(pH))  %>% 
                                    rename(Soil_sample = "Sample_number", 
                                    FAO_soil = "FAO soil classifcation")

hist(soil$pH)
boxplot(pH ~ FAO_soil, soil)

# Loading data & Select variables of interest
crop_soil <- readxl::read_excel(here::here("data", 
"maize", "2012_Joy-mwi-samples.xlsx"), sheet =10, skip =3)  %>%
 select(Sample_number, Soil_sample)  

# Merging soil and sample data

crop  <- crop  %>% left_join(., crop_soil) 

#Checking data
head(crop_soil)
head(soil)

tail(crop)

# Merging with pH info
crop  <- crop  %>% left_join(., soil, by = "Soil_sample")

# Issue with soil names

boxplot(pH ~ FAO_soil.x, crop) 
boxplot(pH ~ FAO_soil.y, crop) 


# Names of the FAO soils have different spelling
crop  %>% filter(!is.na(pH))  %>% select(starts_with("FAO_soil"))  %>% 
distinct()  %>%  View()

soil_name  <- crop  %>% filter(!is.na(pH))  %>% select(starts_with("FAO_soil"))  %>% 
distinct() 

#fixing soil names
for(i in 1:nrow(soil_name)){
crop$FAO_soil.x[crop$FAO_soil.x %in% soil_name[i,1]]  <- soil_name[i,2]
}

crop$FAO_soil  <- unlist(crop$FAO_soil.x)

boxplot(pH ~ FAO_soil, crop) 

crop1 <- crop  %>% filter(!is.na(X_coord) & !is.na(Y_coord))  %>% 
st_as_sf(., coords =c("X_coord", "Y_coord"), crs = "+init=epsg:32736")

#Changing the projection system so it is in line with other datasets (long/lat)
crop1 <- st_transform(crop,  crs = "+init=epsg:4326")

plot(crop1[, "FAO_soil"])
plot(crop1[, "Soil_sample"])

#tm_shape(crop1)+
#tm_symbols(col = "black", "Se" ) +
#tm_legend(show = FALSE) +
tm_shape(crop1)+
tm_symbols(col = "red", "pH") +
tm_legend(show = FALSE) 

