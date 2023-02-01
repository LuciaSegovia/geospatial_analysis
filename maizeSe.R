
library(sf)
library(dplyr)
library(ggplot2)

bn  <- 3

b_admin1  <- st_read(here::here("..", "PhD_geospatial-modelling","data",
 "mwi-boundaries", "gadm40_MWI_1.shp"))

b_admin3  <- st_read(here::here("..", "PhD_geospatial-modelling","data",
 "mwi-boundaries", "gadm40_MWI_3.shp"))

#Checking boundaries

admin = st_intersection(b_admin1, b_admin3) #This one kept the lake
admin = st_intersection(b_admin3, b_admin1)
plot(admin)

grain  <- readxl::read_excel(here::here("..", "GeoNutrition","Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

names(b_admin3)
names(grain)
#plot(b_admin3)
unique(b_admin3$ID_3)

# Maize Se conc. - data manipulation ----

# 1) Subsetting variables of interest: only maize and Se.
se.df <- subset(grain, Crop == "Maize", select = c(Latitude, Longitude, Se_triplequad)) 

# 2) Checking data distribution and re-coding NAs
hist(se.df$Se_triplequad)
se.df$Se_triplequad[se.df$Se_triplequad > 200]  <- NA

# 3) Transforming Se variable (mcg/kg DW) into mcg/100g FW EP for maize
# Maize grain - water content = 13.36g/100g (Hwang et al., 2016) - See documentation for more info. 
water  <- 13.36 
se.df$Se_mcg  <- (se.df$Se_triplequad)*(100-water)/10
hist(se.df$Se_mcg)
names(se.df)

#Removing NA values and 
#converting the dataset into spatial object (sf) 
se  <- subset(se.df, !is.na(Se_triplequad))  %>% 
st_as_sf(., coords =c("Longitude", "Latitude"), crs = "EPSG:4326")

plot(se)

# Getting info on the admin boudaries (district level)
#We changed from name to ID to avoid duplicates
name_var  <- paste0("ID_", bn)
admin  <- b_admin3[, c("NAME_1", name_var, "geometry")]
sum(diplicated(admin$ID_3))
unique(admin$NAME_1)
plot(admin)

# Allocating maize Se values to each district
se_admin = st_intersection(se, admin)
unique(se_admin$NAME_1)

subset(se_admin, NAME_1 == "Balaka")

#Checking Se values per geographic unit
subset(se_admin, NAME_1 == unique(se_admin$NAME_1)[2], Se_triplequad)

# Boxplot of the maize Se values per geographic unit
#TODO: A loop with boxplot for all EAs in each district
var1  <- unique(se_admin$NAME_1)[3]
boxplot(Se_mcg ~ ID_3, data = subset(se_admin, NAME_1 %in% var1), 
        main="Maize Selenium by district",
        xlab=paste0("EAs in ", var1, " distric"), ylab="Se (mcg/100g FW-EP)", pch=19)

# Plotting values (e.g., median Se values) per geographic unit
se_admin  %>% st_drop_geometry()  %>% 
group_by(NAME_1)  %>% 
summarise(Se_median = median(Se_triplequad))   %>% 
left_join(., admin_2)  %>% st_as_sf()  %>% 
plot()

# Plotting Se median (mcg/100g FW-EP) per geographic unit
se_admin  %>% st_drop_geometry()  %>% 
group_by(ID_3)  %>% 
summarise(Se_median = median(Se_mcg, na.rm = T))   %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
ggplot() + 
  geom_sf(aes(fill = Se_median))
