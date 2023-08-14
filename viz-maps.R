



GPS_Se  %>% 
# st_as_sf(., coords = c('LONGNUM', 'LATNUM'))  %>% 
ggplot() + 
  geom_sf(aes(color = selenium))

tm_shape(b_admin1) +
  tm_polygons() +
  tm_shape(GPS_Se) + 
  tm_symbols(col = "black", size = "selenium")

boundaries$shapeID[boundaries$shapeID == "60268647B1308848342151"] 
  boundaries  %>% filter(shapeID != "60268647B1308848342151")  %>% 
  tm_shape() +
  tm_polygons() +
  tm_shape(dhs_se) + 
  tm_symbols(col = "black", size = "selenium")
