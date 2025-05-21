
# install.packages(c("foreach", "doParallel"))

library(foreach) # for parallel processing
library(doParallel) # for parallel processing
library(geosphere) # for calculating distances
library(sf) # for reading in and writting shapefiles
library(dplyr)
library(ggplot2)

# Loading the data
grain  <- readxl::read_excel(here::here("..", "GeoNutrition",
"Geostatistics_and_mapping", "Malawi_Grain.xlsx"))

# Selecting the boundaries (1-3; district to EA)

bn  <- 3

boundaries  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", paste0("gadm40_MWI_", bn, ".shp")))

b_admin1  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "gadm40_MWI_1.shp"))

b_admin3  <- st_read(here::here("..", "PhD_geospatial-modelling", "data",
 "mwi-boundaries", "gadm40_MWI_3.shp"))

#Checking boundaries - it's commented bc it often crashes the laptop
#TODO: removing Lake Malawi (and other water-bodies) from geometry

#admin = st_intersection(b_admin1, b_admin3) #This one kept the lake
#admin = st_intersection(b_admin3, b_admin1)
plot(boundaries)

# Trying to simplify the boundaries
b_admin3_simp  <- st_simplify(b_admin3, preserveTopology = TRUE, dTolerance = 1000)

dev.off()
plot(b_admin3_simp)

plot_map(b_admin3, graticules = TRUE, strokecolor = '#097FB3',
         fillcolor = '#AED3E4')


dim(grain)

names(b_admin3)
names(grain)

#plot(b_admin3)

unique(b_admin3$ID_3)
length(unique(b_admin3$ID_3))

# Maize Se conc. - data manipulation ----

# 1) Subsetting variables of interest: only maize and Se.
se.df <- subset(grain, Crop == "Maize", select = c(Latitude, Longitude, Se_triplequad)) 
dim(se.df)
sum(is.na(se.df$Se_triplequad))

# 2) Checking data distribution and re-coding NAs
hist(se.df$Se_triplequad)
length(se.df$Se_triplequad[se.df$Se_triplequad > 200]) #408
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
dim(se)

saveRDS(se, here::here("data", "inter-output", "maize_se.rds"))

# Getting info on the admin boudaries (EA/district level)
#We changed from name to ID to avoid duplicates
name_var  <- paste0("ID_", bn)
admin  <- boundaries[, c("NAME_1", name_var, "geometry")]
sum(duplicated(admin$ID_3))
unique(admin$NAME_1)
plot(admin)

# Allocating maize Se values to each admin unit
se_admin = st_intersection(se, admin)
tes1  <- st_intersection(st_geometry(se), st_geometry(admin))
nrow(se) == nrow(se_admin)
sum(duplicated(admin$ID_3))


#Checking the results of the interesection
se_check <- se[st_within(se, admin) %>% lengths > 0,]
se_check <- se[st_intersects(se, admin) %>% lengths > 0,]

nrow(se) == nrow(se_check)

# Are there more than one sample for certain admin units?
sum(duplicated(se_admin$ID_3))

# Are all admin units included?
length(admin$ID_3) == length(admin$ID_3)
length(setdiff(admin$ID_3, se_admin$ID_3))

length(se_admin$ID_3[!is.na(se_admin$Se_mcg)])
se_admin$ID_3[se_admin$Se_mcg < 0]

unique(se_admin$NAME_1)

subset(se_admin, NAME_1 == "Balaka")

#Checking Se values per geographic unit
subset(se_admin, NAME_1 == unique(se_admin$NAME_1)[2], Se_triplequad)


# Boxplot of the maize Se values per geographic unit
#TODO: A loop with boxplot for all EAs in each district
var1  <- unique(se_admin$NAME_1)[3]
boxplot(Se_mcg ~ ID_3, data = subset(se_admin, NAME_1 %in% var1), 
        main="Maize Selenium by district",
        xlab=paste0("EAs in ", var1, " distric"), 
        ylab="Se (mcg/100g FW-EP)", pch=19)

se_district  <- se_admin  %>% st_drop_geometry()  %>% 
group_by(ID_1)  %>% 
summarise(Se_median = median(Se_triplequad))

se_ea  <- se_admin  %>% st_drop_geometry()  %>% 
group_by(ID_3)  %>% 
summarise(Se_median = median(Se_triplequad), 
          Se_mean = mean(Se_triplequad))

# Plotting values (e.g., median Se values) per geographic unit

hist(se_ea$Se_median)  
hist(se_ea$Se_mean)  

# se_district 
se_ea %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#plot()
ggplot() + 
  geom_sf(aes(fill = Se_median)) #+
 # scale_fill_gradientn(colours=topo.colors(7),
  #  limits=c(0.009, 0.17))


# Plotting Se median (mcg/100g FW-EP) per geographic unit
se_admin  %>% st_drop_geometry()  %>% 
group_by(across(all_of(name_var))) %>% 
# group_by(ID_1)  %>% 
summarise(Se_median = median(Se_mcg, na.rm = T))   %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#.[1,]  %>% 
ggplot() + 
  geom_sf(aes(fill = Se_median)) 
  

# Trying out the paralellisation ------
results <- list()
# set up a cluster with n cores for parallel processing. I use n - 1 cores to leave one core free for other processes.
cl <- makeCluster(7)

# register the cluster for parallel processing
registerDoParallel(cl)

#
test.df  <- se_admin  %>% st_drop_geometry()  %>% 
group_by(across(all_of(name_var))) %>% 
summarise(Se_median = median(Se_mcg, na.rm = T))   %>% 
full_join(., admin)  %>% st_as_sf()


# loop through the sample points and calculate the pairwise variograms in parallel
 results <- foreach(i = 1:nrow(test.df), .combine = rbind) %dopar% {
    result_list <- list()
 for (j in 1:nrow(test.df)) {
       result  <- plot(test.df[j, 2]) 
        result_list[[j]] <- result
 }
    # print a message to the console to show progress
    message(paste0("Finished ", i, " of ", nrow(test.df), " locations"))
    return(do.call(rbind, result_list))
}

# stop the cluster and deregister the parallel backend
stopCluster(cl)


summary(plasma_admin$Se_median)
summary(se_ea$Se_median)
summary(se_district$Se_median)
summary(re$se_mean)

plot(se_ea$Se_median, re$se_mean,  
     main="",
     xlab="", ylab="", pch=19)

plot(test$selenium, test$Se_triplequad, 
     main="",
     xlab="", ylab="", pch=19)

test  <- plasma_admin  %>% left_join(se_ea, 
by = "ID_3")

sum(is.na(plasma_admin$ID_3))
count(plasma_admin$ID_3)
sum(is.na(plasma_admin$selenium))

sum(is.na(se_admin$Se_triplequad))
count(se_admin$ID_3)


test  <-  plasma_admin   %>% st_drop_geometry()  %>%
left_join(., se_admin  %>%  st_drop_geometry(), 
by = "ID_3")

test  <- plasma_admin  %>% st_join(se_admin) 
