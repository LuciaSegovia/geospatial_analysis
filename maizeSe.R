
library(sf)
library(dplyr)

b_admin1  <- st_read(here::here("..", "PhD_geospatial-modelling","data", "mwi-boundaries", "gadm40_MWI_1.shp"))
grain  <- readxl::read_excel(here::here("..", "GeoNutrition","Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

names(b_admin1)
names(grain)

se.df <- subset(grain, Crop == "Maize", select = c(Latitude, Longitude, Se_triplequad)) 

hist(se.df$Se_triplequad)
se.df$Se_triplequad[se.df$Se_triplequad> 200]  <- NA
se.df$Se[se.df$Se> 500] 
se.df$Se  <- se.df$Se_triplequad*1000
hist(se.df$Se)

se  <- subset(se.df, !is.na(Se), select=c(1:2,4))  %>% 
st_as_sf(., coords =c("Longitude", "Latitude"), crs = "EPSG:4326")

plot(se)

admin_2  <- b_admin1[, c("NAME_1", "geometry")]
unique(b_admin1$ID_1)

plot(admin_2)

se_admin = st_intersection(se, admin_2)

unique(se_admin$NAME_1)

plot(p_xy2)

p_xy2 = st_intersection(p, x_and_y)
sel_p_xy = st_intersects(p, x, sparse = FALSE)[, 1] &
  st_intersects(p, y, sparse = FALSE)[, 1]
p_xy3 = p[sel_p_xy]


subset(se_admin, NAME_1 == unique(se_admin$NAME_1)[2], Se)


boxplot(Se ~ NAME_1, data = se_admin, 
        main="Maize Selenium by district",
        xlab="District", ylab="Maize Se (mcg)", pch=19)
