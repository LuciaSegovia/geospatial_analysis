
# Cleaning the environment
rm(list = ls())

# Loading libraries and functions
#install.packages("rgeos")

library(dplyr) # data wrangling
library(ggplot2) # visualisation
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("functions", "CEPHaStat_3.R")) #stat functions
library(geoR)  # geospatial modelling


## Loading data ----

# Plasma Se conc. (cleaned from 00_cleaning-dhs.R)
plasma.df  <- readRDS(here::here("data", "inter-output","dhs_se_gps.rds")) %>% # cleaned geo-loc plasma Se data
  filter(!is.na(selenium)) # %>% select(1:48) # removing buffer and other spatial vars

names(plasma.df)

# Adding district variable to plasma 

cluster.df <- readRDS(here::here("data", "inter-output", 
                   "aggregation", "master-cluster-admin-level.RDS"))


plasma.df  <- plasma.df %>% left_join(., cluster.df %>% 
                          select(survey_cluster1, ADM2_PCODE, ADM2_EN) %>% 
                          distinct())

# Maize Se conc. (from 01_maize-aggregation.R)

(file <- grep("pred-maize.*._v2", list.files(here::here("data", "inter-output", "aggregation")), 
     value = TRUE))

# Generating the file ----

# Loop to generate a file with plasma data and maize with each aggregation unit


# Run one for every file/aggegation
for(i in 1:length(file)){
  
  # Load the maize Se conc. aggregated dataset
  maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                                 file[i]))
  
  # Join (left) plasma and maize datasets
  #based on common variable (eg cluster id)
  data.df <- left_join(plasma.df, maize.df) 
  
  # Check if there are missing values for plasma Se conc.
  if(sum(is.na(data.df$selenium))>0){
    stop(paste0("Missing values in plamsa Se in ", file[i]))
    
  }
  
  # Check if there are missing values for maize Se conc.
  if(sum(is.na(data.df$Se_mean))>0){
    stop(paste0("Missing values in maize Se in ", file[i]))
    
  }
  
  # Save the output into the model folder
  saveRDS(data.df, here::here("data", "inter-output",   "model",  
                              paste0("plasma-", 
                                     file[i])))
  
}


# END -----

# Checking missing values

data.df %>% filter(is.na(Se_mean))


data_id <- readRDS(here::here("data", "inter-output", 
                              "mwi-plasma-se_maize-admin.RDS")) %>% 
  select(unique_id, survey_cluster1, EACODE, DISTRICT, meter)



names(plasma.df)
names(maize.df)

# Adding the EA info, & removing empty Plasma Se values 
plasma.df <- plasma.df %>% left_join(., data_id) %>% filter(!is.na(selenium))

#Checking distances
hist(plasma.df$meter[plasma.df$urbanity == "2"])

sum(!is.na(plasma.df$meter) & plasma.df$urbanity == "2" & 
      plasma.df$meter >5000) # 8 rural living WRA were located in EA >5km apart

table(plasma.df$urbanity)
length(plasma.df$survey_cluster1[!is.na(plasma.df$meter) &
                                   plasma.df$urbanity == "2" & plasma.df$meter >5000]) # 8 WRA in the same cluster were located in EA >5km apart

plasma.df$unique_id[plasma.df$survey_cluster1 == "497"]
plasma.df$meter[plasma.df$survey_cluster1 == "497"]

# checking cluster were either rural or urban
table(plasma.df$urbanity, plasma.df$survey_cluster1)
plasma.df$survey_cluster1[plasma.df$urbanity == 1 & 
                            plasma.df$urbanity == 2]
# The ids of the rural
rural_id <- unique(plasma.df$survey_cluster1[plasma.df$urbanity == 2])



# Checking that all ids w/ Se values has a "co-located" EACODE. 
plasma.df$EACODE <- as.character(plasma.df$EACODE)

# We can see that there are 2EAs (cluster) w/ NAs that's because those raw values 
# were zero (hence converted into -Inf). 

plasma.df %>% left_join(., maize.df, by = c("EACODE" = "admin_id")) %>% 
  filter(is.na(maizeSe_mean)) %>% distinct(EACODE)

maize.df$maizeSe_mean[maize.df$admin_id %in% c("10320707", "30106072")]

# Combining plasma and EA-level maize

ea.df <- plasma.df %>% left_join(., maize.df, by = c("EACODE" = "admin_id")) 

## REVIEW: We are assinging the min value bc it cannot be zero (log transf. needed)
# If we are leaving it as NA we will be loosing those observations for Se plasma
min(ea.df$maizeSe_mean[!is.na(ea.df$maizeSe_mean)])
ea.df$maizeSe_mean[ea.df$EACODE %in% c("10320707", "30106072")] <- 0.002367136
#ea.df$maizeSe_mean[ea.df$EACODE %in% c("10320707", "30106072")] <- NA
sum(is.na(ea.df$maizeSe_mean))


# Plotting
hist(ea.df$maizeSe_mean)
summaplot(ea.df$maizeSe_mean)
summaplot(log(ea.df$maizeSe_mean))

plot(ea.df$selenium, ea.df$maizeSe_mean)

boxplot(maizeSe_mean ~ region, ea.df)
boxplot(selenium ~ region, ea.df)

names(ea.df)

# Saving final EA dataset
saveRDS(ea.df, here::here("data", "inter-output", "raw-maizeSe_plasma-se_ea.RDS"))

