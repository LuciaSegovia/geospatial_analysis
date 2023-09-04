
#######################################################################
#
#   Cleaning & exploring the DHS data from MNS in Malawi
#     variables of interest: Plasma Se conc., wealth Q, BMI,
#     # urbanity, HH location, district, region,
#
#####################################################################################################


# Loading libraries and functions

library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(survey) # survey design
#options(survey.lonely.psu="adjust") # For Error of one PSU at stage 1
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("CEPHaStat_3.R")) #stat functions

# Loading the datat
dhs.df<- haven::read_dta(here::here("data","MWIR7AFL.dta")) #survey data DHS
Malawi_WRA <- haven::read_dta(here::here("data", "MW_WRA.dta")) #Biomarkers data DHS
b_admin3  <- st_read(here::here("data", "mwi-boundaries", "gadm40_MWI_3.shp")) #EA boundaries
b_admin1  <- st_read(here::here("data", "mwi-boundaries", "gadm40_MWI_1.shp")) #District boundaries

# Import checks
n01 <-  dim(Malawi_WRA)[1]
names(Malawi_WRA)
dim(dhs.df)
#View(Malawi_WRA)
head(dhs.df)
#plot(b_admin1[,1])

# Renaming variables 
Malawi_WRA <-Malawi_WRA %>% dplyr::rename(
  ferritin='fer',
  region='mregion',
  sex = "m04", 
 # rbp='RBP', #correctly labelled
#  crp='CRP', #correctly labelled
 # agp='AGP', #correctly labelled
#  stfr='sTfR',
  zinc='zn_gdl',
  dretinol='incap_dr',
  retinol='incap_retinol',
  urbanity='mtype',
  MRDR='mrdr_ratio',
  survey_cluster1='mcluster',
  household_id1='mnumber',
  survey_weight="mweight",
  LINENUMBER='m01',
  AGE_IN_YEARS='m07',
  SALT_PRESENT='m12',
  SALT_LABEL_IODIZED='m104',
  OIL_PRESENT='m110',
  OIL_FORTIFIED='m112',
  IRON_LWEEK='m414',
  ANY_IRON='m415',
  FEVER24='m417',
  HEMOGLOBIN_LAB='m432',
  Malaria_test_result='m431',
  WEIGHT='m435',
  HEIGHT='m436',
  is_pregnant='preg',
  agp_HIGH='agp_c1',
  crp_HIGH='crp_c1',
  ANY_INFLAMMATION='agp_crp_c1',
  ANEMIA_ADJUSTED='anemia',
  SFER_microg_L_ADJUSTED_INTERNAL='sf_reg',
  LOW_SFER_microg_L_UNADJUSTED='sf_c2',
  LOW_SFER_microg_L_ADJUSTED='sf_c1',
  VITA_CONTENT_OIL='oil_vita',
  VITA_CONTENT_SUGAR='sugar_vita',
  IODINE_SALT='salt',
  was_fasting='fast',
  ps_folate='fol',
  rbc_folate='rbcf',
  iodine='iod',
  selenium='sel',
  vitamin_b12_gr='vitb12', 
  time_of_day_sampled2= "m430h",
  supple = "m415"
 )

#Checking variables
dim(Malawi_WRA)
sum(duplicated(Malawi_WRA$survey_cluster1))
sum(duplicated(Malawi_WRA$LINENUMBER))
table(Malawi_WRA$LINENUMBER)

Malawi_WRA$unique_id  <- paste0(Malawi_WRA$survey_cluster1,
                        Malawi_WRA$household_id1, Malawi_WRA$LINENUMBER)

# Creating a unique id for each WRA
sum(duplicated(Malawi_WRA$unique_id))

# Checking survey weight & generating the variable
# MNS Weight survey
unique(Malawi_WRA$survey_weight)
sum(is.na(Malawi_WRA$survey_weight)) #All observations have weight
sum(Malawi_WRA$survey_weight==0) #All observations have weight > 0
Malawi_WRA$wt  <- Malawi_WRA$survey_weight/1000000
hist(Malawi_WRA$survey_weight)

#Checking high weights & Se values for those high weights
Malawi_WRA$survey_weight[Malawi_WRA$survey_weight>5000000]
Malawi_WRA$selenium[Malawi_WRA$survey_weight>5000000]

summaplot(Malawi_WRA$wt)
table(Malawi_WRA$wt)

#Checking that survey weight are representative of pop. (WRA)
# Perc. difference with population (WRA)
(n01-sum(Malawi_WRA$wt))/n01*100


# Description of the sample ----

# Looking at variables of interest: summary stats + histogram

#Age - Women of reproductive age (15-49 years)
# REVIEW: Not excluding outside WRA age range
# Note: That in the histogram women = 15yo are counted in the first
# bar (that's why the freq. is over 50)
sum(!is.na(Malawi_WRA$AGE_IN_YEARS)) #no missing values
hist(Malawi_WRA$AGE_IN_YEARS)
summary(Malawi_WRA$AGE_IN_YEARS)
sum(Malawi_WRA$AGE_IN_YEARS<15 | Malawi_WRA$AGE_IN_YEARS>49) #8 younger than range
subset(Malawi_WRA, AGE_IN_YEARS<15 | AGE_IN_YEARS>49,
select = c(region, urbanity, selenium)) #8 younger than range

# Age Weighted mean by region/urbanity
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(AGE_IN_YEARS, wt,na.rm = T))
ddply(Malawi_WRA,~urbanity,summarise,median=matrixStats::weightedMedian(AGE_IN_YEARS, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(AGE_IN_YEARS, wt,na.rm = T))

#Sex - Female == 2 & pregnancy
unique(Malawi_WRA$sex)
which(Malawi_WRA$sex==1) #Label as male
# REVIEW: Changing coded "men" to "women"
Malawi_WRA$sex[Malawi_WRA$sex==1]  <- 2 #converting into women

# Pregnancy
dim(Malawi_WRA)
unique(Malawi_WRA$is_pregnant)
which(Malawi_WRA$is_pregnant==1) #Label as pregnant
length(which(Malawi_WRA$is_pregnant==1)) #Label as pregnant
subset(Malawi_WRA, is_pregnant==1, 
select = c(region, urbanity #, selenium
))  %>%  count()
subset(Malawi_WRA, is_pregnant==0 | is.na(is_pregnant))  %>% dim()
# REVIEW: Removing the pregnant women
Malawi_WRA  <- subset(Malawi_WRA, is_pregnant==0 | is.na(is_pregnant)) 

#Height - ouliers (converting 999 to NA)
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT >200]
# REVIEW: Outliers 999 to NA
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT >999] <- NA
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT <130] 
Malawi_WRA$AGE_IN_YEARS[Malawi_WRA$HEIGHT <120] 

# height Weighted mean by region
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(HEIGHT, wt,na.rm = T))

Malawi_WRA$urbanity  <- as.factor(Malawi_WRA$urbanity)
class(Malawi_WRA$region)
ggplot(Malawi_WRA, aes(x = as.factor(region), y = selenium)) +   
geom_boxplot()

#Weight - ouliers (converting 999 to NA)
hist(Malawi_WRA$WEIGHT)
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >200]
Malawi_WRA$region[Malawi_WRA$WEIGHT >999] # Checking if this "missing values" are affecting more to a particular region
Malawi_WRA$urbanity[Malawi_WRA$WEIGHT >999] # Same but for urbanity
# REVIEW: Outliers 999 to NA
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >999] <- NA
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >80] 
Malawi_WRA$HEIGHT[Malawi_WRA$WEIGHT >80] 
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT <40] 
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT <40 & Malawi_WRA$AGE_IN_YEARS >15] 
Malawi_WRA$HEIGHT[Malawi_WRA$WEIGHT <40 & Malawi_WRA$AGE_IN_YEARS >15] 

# weight Weighted mean by region/ urbanity
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(WEIGHT, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(WEIGHT, wt,na.rm = T))
ddply(Malawi_WRA,.(urbanity), summarise,median=matrixStats::weightedMedian(WEIGHT, wt,na.rm = T))


#creating BMI variable
Malawi_WRA$BMI<- Malawi_WRA$WEIGHT/(Malawi_WRA$HEIGHT/100)^2

#BMI - 
hist(Malawi_WRA$BMI)
summary(Malawi_WRA$BMI)
summaplot(Malawi_WRA$BMI)
sum(Malawi_WRA$BMI<15 | Malawi_WRA$BMI>25)
Malawi_WRA$BMI[which(Malawi_WRA$BMI>40)]
sum(Malawi_WRA$BMI[Malawi_WRA$BMI>40])
Malawi_WRA[298, "WEIGHT" ] #Weight & BMI outlier

ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(BMI, wt,na.rm = T))

# Selenium
sum(is.na(Malawi_WRA$selenium)) # Checking NA

# WRA location
sum(is.na(Malawi_WRA$region)) # Checking NA
sum(is.na(Malawi_WRA$urbanity)) # Checking NA

#Checking numeric variables
#selenium, HEIGHT, WEIGHT
var <- "selenium"
x <- pull(Malawi_WRA[, var])

hist(x, main = paste("Histogram of",  tolower(var)) , xlab = var)
summary(x)
#Skewness
summa(x)
summaplot(x)

# Se Weighted mean/median by region/urbanity
ddply(Malawi_WRA,~region, summarise,mean=weighted.mean(selenium, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(selenium, wt,na.rm = T))

#Boxplot Se ~ U/R and Region
boxplot(selenium ~ urbanity*region, data = Malawi_WRA,
        col = c("white", "steelblue"), frame = FALSE)

#Boxplot Se ~ Malaria and Region
boxplot(selenium ~ Malaria_test_result*region, data = Malawi_WRA,
        col = c("white", "steelblue"), frame = FALSE)

# Individual level - source of variability 

# Selenium & Age
plot(Malawi_WRA$selenium, Malawi_WRA$AGE_IN_YEARS, 
     main="Selenium by Age",
     xlab="Selenium", ylab="Age", pch=19)

 # Selenium & BMI
plot(Malawi_WRA$selenium, Malawi_WRA$BMI,
     main="Selenium by BMI",
     xlab="Selenium", ylab="BMI", pch=19)

# Selenium & CRP (Inflamation)
boxplot(selenium ~ crp_HIGH, Malawi_WRA)

# Checking DHS survey data  ------
# (Wealth index and other variables)
dim(dhs.df)

DHSDATA<- dhs.df %>% dplyr::rename( 
  survey_cluster1='v001',
  household_id1='v002',
  LINENUMBER='v003', 
  region = 'v101', 
  wealth_quintile= "v190",
  Literacy= "v155",
  education_level= "v106"
)


# Socio-economic data exploration:

# wealth quintile poorest-richest == 1-5
hist(DHSDATA$wealth_quintile)
summary(DHSDATA$wealth_quintile)
plot(DHSDATA$wealth_quintile, DHSDATA$education_level, 
     main="Wealth vs Education",
     xlab="Wealth Q", ylab="Education level", pch=19)
	
# education 0-3 (low to high)
plot(DHSDATA$wealth_quintile, DHSDATA$education_level, 
     main="Wealth vs Education",
     xlab="Wealth Q", ylab="Education level", pch=19)

# literacy 0-2 (cant to can read) (3,4 others)
plot(DHSDATA$education_level, DHSDATA$Literacy,  
     main="Education vs Literacy",
     xlab="Education level", ylab="Literacy level", pch=19)

#Checking entries w/ no literacy but secondary educ. (n=36)
sum(DHSDATA$education_level >1 & DHSDATA$Literacy == 0)
DHSDATA$education_level[DHSDATA$education_level >1 & DHSDATA$Literacy == 0]

# region 1-3 (N->S)
plot(DHSDATA$wealth_quintile, DHSDATA$region, 
     main="Wealth vs Region",
     xlab="Wealth Q", ylab="Region", pch=19)

plot(DHSDATA$education_level, DHSDATA$region, 
     main="Education vs Region",
     xlab="Education", ylab="Region", pch=19)

DHSDATA %>% group_by (region) %>% dplyr::count(wealth_quintile)

# Merging with DHS dataset ----
# Merging with DHS dataset to obtain Wealth index and other variables 

EligibleDHS <- Malawi_WRA %>% 
 left_join(., DHSDATA) %>% dplyr::rename(
  #person_id=WomenID,
  is_lactating= "v404", #breastfeeding yes=1, no=0
  is_smoker= "v463a", # Only cover cigarettes (other smoking variables)
  survey_strata= "v022", 
  had_fever='m416',
  had_diarrhea= 'm419',
  had_malaria= 'm420')  



dhs_varibles  <- c("household_id1", "LINENUMBER",
"region",
"wealth_quintile",
"Literacy",
"education_level", 
"is_lactating",
"is_smoker",
"survey_strata",
"had_fever",
"had_diarrhea",
"had_malaria", 
"sdist")

# Selecting variables
women  <- Malawi_WRA %>% dplyr::select(c(1:14, 21:23,
 25, 27, 92, 96,97, 117, 131:162))  %>% names()

#Checking if intoducing duplicates
n01 == dim(EligibleDHS)[1] # need to be minus pregnant
dim(Malawi_WRA)[1] == dim(EligibleDHS)[1] 

ddply(EligibleDHS, ~wealth_quintile, summarise,medianBMI=matrixStats::weightedMedian(BMI, wt,na.rm = T))


# Data checks (non-weigheted)

#Cluster survey
unique(EligibleDHS$survey_cluster1)
sum(is.na(EligibleDHS$survey_cluster1)) #All observations have cluster
sum(EligibleDHS$survey_cluster1==0) #All observations have weight > 0

#household ID
unique(EligibleDHS$household_id1)
sum(is.na(EligibleDHS$household_id1)) #All observations have HH ID
sum(EligibleDHS$household_id1==0) #All observations have weight > 0

#Wealth Quantile
unique(EligibleDHS$wealth_quintile)
sum(is.na(EligibleDHS$wealth_quintile)) #29 observations are missing WQ

boxplot(selenium ~ wealth_quintile, data = EligibleDHS, 
     main="Plasma Selenium by Wealth Quintile",
     xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19)

boxplot(selenium ~ urbanity*wealth_quintile, data = EligibleDHS, 
     main="Plasma Selenium by Wealth Quintile",
     xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19)

boxplot(BMI ~ urbanity*wealth_quintile, data = EligibleDHS, 
     main="BMI by Wealth Quintile",
     xlab="Wealth Quantile", ylab="BMI (kg/m^2)", pch=19)

(x  <- boxplot(BMI ~ wealth_quintile, data = EligibleDHS, 
     main="BMI by Residency",
     xlab="Residency", ylab="BMI (kg/m^2)", pch=19))

x$stats[3,]

#Education level
unique(EligibleDHS$education_level)
sum(is.na(EligibleDHS$education_level)) #29 observations are missing Educ. 

boxplot(selenium ~ education_level, data = EligibleDHS, 
        main="Plasma Selenium by Education level",
        xlab="Education level", ylab="plasma Se (ng/ml)", pch=19)

#Literacy
unique(EligibleDHS$Literacy)
sum(is.na(EligibleDHS$Literacy)) #29 observations are missing Educ. 

boxplot(selenium ~ Literacy, data = EligibleDHS, 
        main="Plasma Selenium by Literacy level", # nolint
        xlab="Literacy level", ylab="plasma Se (ng/ml)", pch=19)

#Region
unique(EligibleDHS$region)
sum(is.na(EligibleDHS$region)) #All observations have region 
table(EligibleDHS$region)

boxplot(selenium ~ region , data = EligibleDHS, 
        main="Plasma Selenium by Region",
        xlab="Region", ylab="plasma Se (ng/ml)", pch=19)

# District
unique(EligibleDHS$sdist)
sum(is.na(EligibleDHS$sdist)) #29 observations are missing
table(EligibleDHS$sdist)

boxplot(selenium ~ sdist , data = EligibleDHS, 
        main="Plasma Selenium by District",
        xlab="District", ylab="plasma Se (ng/ml)", pch=19)

# Smoking
unique(EligibleDHS$is_smoker)
sum(is.na(EligibleDHS$is_smoker)) #29 observations are missing smoking status
sum(EligibleDHS$is_smoker == 1 & !is.na(EligibleDHS$is_smoker))
boxplot(selenium ~ is_smoker , data = EligibleDHS, 
        main="Plasma Selenium by Smoking status",
        xlab="Smoking", ylab="plasma Se (ng/ml)", pch=19)

#Identifying HH ID & cluster of missing socio-eco info
subset(EligibleDHS, is.na(EligibleDHS$wealth_quintile), 
       select = c("household_id1","survey_cluster1")) %>% left_join(.,DHSDATA) %>% 
  group_by(survey_cluster1, household_id1) %>% dplyr::count(wealth_quintile) %>% 
  filter(!is.na(wealth_quintile)) %>% 
  View()

#Sorting Wealth Q for each HH ID & cluster of missing info
Wealth <- subset(EligibleDHS, is.na(EligibleDHS$wealth_quintile), 
       select = c("household_id1","survey_cluster1")) %>% 
   left_join(., DHSDATA %>% 
             select(c("household_id1","survey_cluster1", "wealth_quintile"))) %>% 
  group_by(survey_cluster1, household_id1) %>%
  filter(!is.na(wealth_quintile)) %>% 
  distinct()

#Checking rows with missing value for wealth Q
n <- which(is.na(EligibleDHS$wealth_quintile))

#Checking line id (hh member id) with & w/o missing value for wealth Q w/ same 
#H ID & cluster ID as in wealth df
EligibleDHS$LINENUMBER[#is.na(EligibleDHS$wealth_quintile) &
  EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[1] &
                              EligibleDHS$household_id1 %in% Wealth$household_id1[1]]

#Generate a loop to assign Wealth Q from other member of the HH to non-reported people in biomarker survey data
for(i in seq_along(Wealth$household_id1)){
  
EligibleDHS$wealth_quintile[EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[i] &
                              EligibleDHS$household_id1 %in% Wealth$household_id1[i]] <- Wealth$wealth_quintile[i]
}

#Checking results from the loop. 

DHSDATA$wealth_quintile[
  DHSDATA$survey_cluster1 %in% Wealth$survey_cluster1[10] &
    DHSDATA$household_id1 %in% Wealth$household_id1[10]]

DHSDATA$LINENUMBER[
  DHSDATA$survey_cluster1 %in% Wealth$survey_cluster1[10] &
    DHSDATA$household_id1 %in% Wealth$household_id1[10]]

Malawi_WRA$LINENUMBER[
  Malawi_WRA$survey_cluster1 %in% Wealth$survey_cluster1[10] &
    Malawi_WRA$household_id1 %in% Wealth$household_id1[10]]

EligibleDHS$LINENUMBER[
  EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[10] &
    EligibleDHS$household_id1 %in% Wealth$household_id1[10]]

EligibleDHS$wealth_quintile[
  EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[10] &
    EligibleDHS$household_id1 %in% Wealth$household_id1[10]]

subset(EligibleDHS, !is.na(selenium), 
select = c(v005, survey_weight))

# TODO: Defining Se deficiency
EligibleDHS$LOW_SEL_GPx3 <- ifelse(EligibleDHS$selenium<84.9,1,0)
EligibleDHS$LOW_SEL_IDI <- ifelse(EligibleDHS$selenium<64.8,1,0)
EligibleDHS$LOW_SEL_KD <- ifelse(EligibleDHS$selenium<30,1,0)


# Saving Se dataset into R object
saveRDS(EligibleDHS,  file=here::here("data", "inter-output","dhs.rds"))

EligibleDHS  <- readRDS(here::here("data","inter-output","dhs.rds")) 

# add GPS values
GPS <- st_read(here::here("data", "MWGE7AFL", "MWGE7AFL.shp")) #GPS location DHS
GPS<-dplyr::rename(GPS, survey_cluster1='DHSCLUST')
dim(GPS)
# Merging with the dataset
EligibleDHS <- merge(EligibleDHS, GPS, by='survey_cluster1')

EligibleDHS<-dplyr::rename(EligibleDHS, Latitude='LATNUM')
 EligibleDHS<-dplyr::rename(EligibleDHS, Longitude='LONGNUM')
EligibleDHS <- dplyr::rename(EligibleDHS, altitude_in_metres='ALT_GPS')

# Only for Se in the dataset
# GPS_Se <- merge(EligibleDHS[, c("unique_id", "survey_cluster1", "sdist",  "selenium", "wealth_quintile", "region")], GPS, by='survey_cluster1')
# GPS_Se  <- st_as_sf(EligibleDHS[, c("unique_id", "survey_cluster1", "sdist", "urbanity",  "selenium", "survey_weight",
 # "wealth_quintile", "BMI", "" "region")], crs = st_crs(4326), coords = c('LONGNUM', 'LATNUM'))
 
 # Maps-testing: Visualising Se conc.
EligibleDHS  %>% 
 st_as_sf(., coords = c('Longitude', 'Latitude'))  %>% 
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
 # Saving Se dataset into R object
#saveRDS(GPS_Se, file=here::here("data", "inter-output","dhs_se.rds"))

# Saving DHS + GPS dataset into R object
saveRDS(EligibleDHS, file=here::here("data", "inter-output","dhs-gps.rds"))

# Applying survey weight
EligibleDHS  <- readRDS(file=here::here("data", "inter-output","dhs-gps.rds"))
sum(is.na(EligibleDHS$selenium))
EligibleDHS  <- subset(EligibleDHS, !is.na(selenium))

class(EligibleDHS$urbanity)

EligibleDHS$survey_cluster1  <- as.factor(EligibleDHS$survey_cluster1)
EligibleDHS$wealth_quintile  <- as.factor(haven::zap_labels(EligibleDHS$wealth_quintile))
EligibleDHS$sdist <- haven::zap_labels(EligibleDHS$sdist)
EligibleDHS$region  <- haven::zap_labels(EligibleDHS$region)

EligibleDHS$wealth_quintile <- factor(EligibleDHS$wealth_quintile,
    levels=c (1,2,3,4,5), 
                    labels=c('Lowest Wealth Q','Low Wealth Q',
                     'Middle Wealth Q', "High Wealth Q", "Highest Wealth Q"), ordered=TRUE)


table(EligibleDHS$urbanity)
table(EligibleDHS$URBAN_RURA)
table(EligibleDHS$had_malaria)
table(EligibleDHS$Malaria_test_result)
table(EligibleDHS$ANY_INFLAMMATION)

# Complex sample design parameters

DHSdesign<-svydesign(id=EligibleDHS$survey_cluster1, 
strata=EligibleDHS$survey_strata, #This strata
 weights=EligibleDHS$survey_weight, data=EligibleDHS)


# tabulate indicator by region
svyby(~selenium, ~wealth_quintile,  DHSdesign, svymean, vartype=c("se","ci"))
svyby(~selenium, ~urbanity,  DHSdesign, svymean, vartype=c("se","ci"))
svyby(~selenium, ~urbanity*wealth_quintile,  DHSdesign, svymean, vartype=c("se","ci"))

svyhist(~selenium,   DHSdesign)

svyboxplot(selenium~urbanity,   DHSdesign)
svyboxplot(selenium~wealth_quintile,   DHSdesign)
svyboxplot(selenium~sdist,   DHSdesign)
svyboxplot(selenium~region,   DHSdesign)



