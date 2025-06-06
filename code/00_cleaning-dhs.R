
################################################################################
#
#        Cleaning & exploring the DHS data from MNS in Malawi
#        variables of interest: Plasma Se conc., wealth Q, BMI,
#             urbanity, HH location, district, region.
#
################################################################################
# Setting up ----

# Cleaning the environment
rm(list = ls())

# Loading libraries and functions
library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(survey) # survey design
library(srvyr) # survey design 2
#options(survey.lonely.psu="adjust") # For Error of one PSU at stage 1
library(sf) # spatial data manipulation
#library(tmap)  #spatial data manipulation and visualisation
source(here::here("functions", "CEPHaStat_3.R")) #stat functions

# Loading the data
dhs.df<- haven::read_dta(here::here("data","MWIR7AFL.dta")) # survey data DHS
Malawi_WRA <- haven::read_dta(here::here("data", "MW_WRA.dta")) #Biomarkers data DHS
# b_admin3  <- st_read(here::here("data", "mwi-boundaries", "gadm40_MWI_3.shp")) #EA boundaries
# b_admin1  <- st_read(here::here("data", "mwi-boundaries", "gadm40_MWI_1.shp")) #District boundaries
# Reading the EA shapefile w/ updated districts (See 00_cleaning-boundaries.R)
ea_admin <- st_read(here::here( "data", "inter-output", 
                                "boundaries", "mwi_admbnda_adm4_nso.shp"))

table(Malawi_WRA$mcluster)

# Import checks
n01 <-  dim(Malawi_WRA)[1]
names(Malawi_WRA)
dim(dhs.df)
#View(Malawi_WRA)
head(dhs.df)
#plot(b_admin1[,1])

# MNS data ----

# Renaming variables 
Malawi_WRA <- Malawi_WRA %>% dplyr::rename(
  ferritin='fer',
  region='mregion',
  sex = "m04", 
 # rbp='RBP', #correctly labelled
#  crp='CRP', # inflammation biomarker
 # agp='AGP', # inflammation biomarker
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
  supple ='m415', # took any supplements
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
had_fever='m416',
had_diarrhea= 'm419',
had_malaria= 'm420') 

# Selecting variables of interest:

Malawi_WRA <- Malawi_WRA %>% dplyr::select(region, sex , crp,  agp, urbanity, 
                                    survey_cluster1,
household_id1, survey_weight, LINENUMBER, AGE_IN_YEARS, supple, # took any supplements
Malaria_test_result, WEIGHT, HEIGHT, is_pregnant, selenium) 

# Checking variables
str(Malawi_WRA)

# Formatting variables ----
## Checking class of discrete variables (imported from stata)
# Region
class(Malawi_WRA$region)
Malawi_WRA$region  <- as.factor(Malawi_WRA$region)

# Sex
class(Malawi_WRA$sex)
Malawi_WRA$sex  <- as.factor(Malawi_WRA$sex)

# Urbanity
class(Malawi_WRA$urbanity)
table(Malawi_WRA$urbanity)
Malawi_WRA$urbanity  <- as.factor(Malawi_WRA$urbanity)
sum(is.na(Malawi_WRA$urbanity))

# Taking supplements - maybe exclude bc only 13 reported consuming and high NA
class(Malawi_WRA$supple)
table(Malawi_WRA$supple)
hist(Malawi_WRA$supple)
sum(is.na(Malawi_WRA$supple)) # 30 missing values (only 13 yes)
Malawi_WRA$supple  <- as.factor(Malawi_WRA$supple)

# Pregnancy
class(Malawi_WRA$is_pregnant)
hist(Malawi_WRA$is_pregnant)
sum(is.na(Malawi_WRA$is_pregnant))
Malawi_WRA$is_pregnant  <- as.factor(Malawi_WRA$is_pregnant)

# Malaria test (yes/no)
class(Malawi_WRA$Malaria_test_result)
sum(is.na(Malawi_WRA$Malaria_test_result)) # 28 NA
hist(Malawi_WRA$Malaria_test_result)
table(Malawi_WRA$Malaria_test_result)

# Recoding Malaria ----
# others values into NA 
Malawi_WRA$Malaria_test_result <- ifelse(Malawi_WRA$Malaria_test_result==4| 
                                        Malawi_WRA$Malaria_test_result==6, 
                                         NA, Malawi_WRA$Malaria_test_result)
Malawi_WRA$Malaria_test_result  <- as.factor(Malawi_WRA$Malaria_test_result)

# survey ids
dim(Malawi_WRA)
sum(duplicated(Malawi_WRA$survey_cluster1))
sum(duplicated(Malawi_WRA$LINENUMBER))
table(Malawi_WRA$LINENUMBER)

# Creating a unique id for each WRA ----
Malawi_WRA$unique_id  <- paste0(Malawi_WRA$survey_cluster1,
                        Malawi_WRA$household_id1, Malawi_WRA$LINENUMBER)

# Checking the unique id
sum(duplicated(Malawi_WRA$unique_id))

# MNS Weight survey ----
# Checking survey weight & generating the variable
unique(Malawi_WRA$survey_weight)
sum(is.na(Malawi_WRA$survey_weight)) # All observations have weight
sum(Malawi_WRA$survey_weight==0) # All observations have weight > 0
Malawi_WRA$wt  <- Malawi_WRA$survey_weight/1000000
hist(Malawi_WRA$survey_weight)

# Checking high weights & Se values for those high weights
Malawi_WRA$survey_weight[Malawi_WRA$survey_weight>5000000]
Malawi_WRA$selenium[Malawi_WRA$survey_weight>5000000]

summaplot(Malawi_WRA$wt)
table(Malawi_WRA$wt)

# Checking that survey weight are representative of pop. (WRA)
# Perc. difference with population (WRA)
(n01-sum(Malawi_WRA$wt))/n01*100

## Description of the sample ----

# Looking at variables of interest: summary stats + histogram
## Age - Women of reproductive age (15-49 years) ----
# REVIEW: Not excluding outside WRA age range
# Note: That in the histogram women = 15yo are counted in the first
# bar (that's why the freq. is over 50)
sum(is.na(Malawi_WRA$AGE_IN_YEARS)) # No missing values
hist(Malawi_WRA$AGE_IN_YEARS)
summary(Malawi_WRA$AGE_IN_YEARS)
sum(Malawi_WRA$AGE_IN_YEARS<15 | Malawi_WRA$AGE_IN_YEARS>49) # 8 younger than range
subset(Malawi_WRA, AGE_IN_YEARS<15 | AGE_IN_YEARS>49,
select = c(region, urbanity, selenium)) # 8 younger than range

# Testing significance (w/o weights)
Malawi_WRA%>% rstatix::anova_test(AGE_IN_YEARS ~ region)
Malawi_WRA%>%
  rstatix::pairwise_t_test(AGE_IN_YEARS ~ region, p.adjust.method = "bonferroni")

Malawi_WRA %>%
  group_by(urbanity, region) %>%
  rstatix::shapiro_test(AGE_IN_YEARS)

# Age Weighted mean by region/urbanity
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(AGE_IN_YEARS, wt,na.rm = T))
ddply(Malawi_WRA,~urbanity,summarise,median=matrixStats::weightedMedian(AGE_IN_YEARS, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(AGE_IN_YEARS, wt,na.rm = T))
ddply(Malawi_WRA,.(region, Malaria_test_result), summarise,median=matrixStats::weightedMedian(AGE_IN_YEARS, wt,na.rm = T))

## Sex  ----
# Female == 2 
unique(Malawi_WRA$sex)
which(Malawi_WRA$sex==1) #Label as male
# REVIEW: Changing coded "men" to "women"
Malawi_WRA$sex[Malawi_WRA$sex==1]  <- 2 #converting into women

## Pregnancy ----
dim(Malawi_WRA)
unique(Malawi_WRA$is_pregnant)
which(Malawi_WRA$is_pregnant==1) #Label as pregnant
length(which(Malawi_WRA$is_pregnant==1)) #Label as pregnant
subset(Malawi_WRA, is_pregnant==1, 
select = c(region, urbanity #, selenium
))  %>%  count()
subset(Malawi_WRA, is_pregnant==0 | is.na(is_pregnant))  %>% dim()
# REVIEW: Removing the pregnant women and unknown status
Malawi_WRA  <- subset(Malawi_WRA, is_pregnant==0 | is.na(is_pregnant)) 

## Height ----
# Checking outliers 
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT >200]
# REVIEW: Missing values (999 to NA)
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT >999] <- NA
Malawi_WRA$HEIGHT[Malawi_WRA$HEIGHT <130] 
Malawi_WRA$AGE_IN_YEARS[Malawi_WRA$HEIGHT <120] 

# Height Weighted mean by region
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(HEIGHT, wt, na.rm = T))

## Weight ----
# Checking outliers (missing values (999) to NA)
hist(Malawi_WRA$WEIGHT)
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >200]
Malawi_WRA$region[Malawi_WRA$WEIGHT >999] # Checking if this "missing values" are affecting more to a particular region
Malawi_WRA$urbanity[Malawi_WRA$WEIGHT >999] # Same but for urbanity

# REVIEW: Missing values (999 to NA)
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >999] <- NA
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT >80] # Checking Weight >80kg
Malawi_WRA$HEIGHT[Malawi_WRA$WEIGHT >80] # Checking height >80Kg
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT <40] # Checking weight <40Kg
Malawi_WRA$WEIGHT[Malawi_WRA$WEIGHT <40 & Malawi_WRA$AGE_IN_YEARS >15] # Checking weight <40Kg if younger
Malawi_WRA$HEIGHT[Malawi_WRA$WEIGHT <40 & Malawi_WRA$AGE_IN_YEARS >15] 


# Weight weighted mean by region/ urbanity
ddply(Malawi_WRA,~urbanity,summarise,mean=weighted.mean(WEIGHT, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(WEIGHT, wt,na.rm = T))
ddply(Malawi_WRA,.(urbanity), summarise,median=matrixStats::weightedMedian(WEIGHT, wt,na.rm = T))


## BMI --------
#Creating BMI variable
Malawi_WRA$BMI<- Malawi_WRA$WEIGHT/(Malawi_WRA$HEIGHT/100)^2

# Checking variable
hist(Malawi_WRA$BMI)
summary(Malawi_WRA$BMI)
summaplot(Malawi_WRA$BMI)
sum(Malawi_WRA$BMI<15 | Malawi_WRA$BMI>25)
Malawi_WRA$BMI[which(Malawi_WRA$BMI>40)]
sum(Malawi_WRA$BMI[Malawi_WRA$BMI>40])
Malawi_WRA[298, "WEIGHT" ] #Weight & BMI outlier

# Generating BMI categories for visualisation & analysis 

Malawi_WRA <- Malawi_WRA%>% 
  mutate(BMI_cat = as.factor(case_when(
    BMI<18.5 ~ "low",
    BMI>18.5 & BMI <24.5 ~ "normal",
    BMI>24.5 ~ "high")),
    BMI_cat = forcats::fct_relevel(BMI_cat, "low", "normal", "high"))
<<<<<<< HEAD:code/00_cleaning-dhs.R

ddply(Malawi_WRA, ~ BMI_cat, summarise, median=matrixStats::weightedMedian(BMI, wt,na.rm = T))
ddply(Malawi_WRA, ~ region, summarise, median=matrixStats::weightedMedian(BMI, wt,na.rm = T))

#Which BMI
Malawi_WRA%>%
  rstatix::pairwise_t_test(BMI ~ region, p.adjust.method = "bonferroni")

=======

ddply(Malawi_WRA, ~ BMI_cat, summarise, median=matrixStats::weightedMedian(BMI, wt,na.rm = T))
ddply(Malawi_WRA, ~ region, summarise, median=matrixStats::weightedMedian(BMI, wt,na.rm = T))

#Which BMI
Malawi_WRA%>%
  rstatix::pairwise_t_test(BMI ~ region, p.adjust.method = "bonferroni")

>>>>>>> 504882a58c86206e93b476f69f95a695e890ab42:00_cleaning-dhs.R
Malawi_WRA%>%
group_by(region) %>% 
    rstatix::pairwise_t_test(BMI ~ urbanity, p.adjust.method = "bonferroni")

## Selenium -----
sum(is.na(Malawi_WRA$selenium)) # Checking NA

ggplot(Malawi_WRA, aes(x = as.factor(region), y = selenium)) +   
  geom_boxplot()

# WRA location
sum(is.na(Malawi_WRA$region)) # Checking NA
sum(is.na(Malawi_WRA$urbanity)) # Checking NA

# Checking numeric variables
# selenium, HEIGHT, WEIGHT, crp, agp
var <- "agp"
x <- pull(Malawi_WRA[, var])

hist(x, main = paste("Histogram of",  tolower(var)) , xlab = var)
summary(x)

# Skewness
summa(x)
summaplot(x)

# Se Weighted mean/median by region/urbanity
ddply(Malawi_WRA,~region, summarise,mean=weighted.mean(selenium, wt,na.rm = T))
ddply(Malawi_WRA,~region, summarise,median=matrixStats::weightedMedian(selenium, wt,na.rm = T))
ddply(Malawi_WRA,.(region, urbanity), summarise,median=matrixStats::weightedMedian(selenium, wt,na.rm = T))
ddply(Malawi_WRA,.(region, Malaria_test_result), summarise,median=matrixStats::weightedMedian(selenium, wt,na.rm = T))

# Se & BMI
ddply(Malawi_WRA, ~ BMI_cat, summarise, median=matrixStats::weightedMedian(selenium, wt,na.rm = T))
ddply(Malawi_WRA,.(region, BMI_cat), summarise,median=matrixStats::weightedMedian(selenium, wt,na.rm = T))

Malawi_WRA$log_se <- log(Malawi_WRA$selenium)

# Normality test
Malawi_WRA %>%
  group_by(urbanity, region) %>%
  # group_by(Malaria_test_result, region) %>%
  rstatix::shapiro_test(selenium)

# Differences between residency
# Region
Malawi_WRA %>% 
  rstatix::anova_test(log_se ~ region)

#Which regions
Malawi_WRA%>%
  rstatix::pairwise_t_test(log_se ~ region, p.adjust.method = "bonferroni")

# Region + Residency
Malawi_WRA %>% 
  group_by(region) %>% 
  rstatix::anova_test(log_se ~ urbanity)

Malawi_WRA%>%
  group_by(region) %>%
  rstatix::pairwise_t_test(log_se ~ urbanity, p.adjust.method = "bonferroni")

# Differences between BMI category
Malawi_WRA %>% 
  rstatix::anova_test(log_se ~ BMI_cat)

#Which BMI
Malawi_WRA%>%
  rstatix::pairwise_t_test(log_se ~ BMI_cat, p.adjust.method = "bonferroni")
# By region, urbanity
Malawi_WRA%>%
  group_by(urbanity) %>%
  rstatix::pairwise_t_test(log_se ~ BMI_cat, p.adjust.method = "bonferroni")


# Boxplot Se ~ U/R and Region
boxplot(selenium ~ urbanity*region, data = Malawi_WRA,
        col = c("white", "steelblue"), frame = FALSE)

# Boxplot Se ~ Malaria and Region
boxplot(selenium ~ Malaria_test_result*region, data = Malawi_WRA,
        col = c("white", "steelblue"), frame = FALSE)

# Boxplot Se ~ suppl and Region
boxplot(selenium ~ supple, data = Malawi_WRA,
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

# Selenium & CRP/ AGP (Inflammation) - log them bc very skewness (check line 250)
plot(log(Malawi_WRA$selenium) ~ log(Malawi_WRA$crp))
plot(log(Malawi_WRA$selenium) ~ log(Malawi_WRA$agp))

# Final check on the number of variables & entries.
dim(Malawi_WRA)

# DHS survey data  ------
# (Wealth index and other variables)

dim(dhs.df)

# Renaming and selecting variables of interest (reducing the number of variables)

DHSDATA <- dhs.df %>% dplyr::rename( 
  survey_cluster1='v001',
  household_id1='v002',
  LINENUMBER='v003', 
  survey_strata= "v022",
  PSU = "v021", 
  region = 'v101', 
  urbanity = "v102", 
  # region = 'v024', 
  age_year = "v012", # for consistency 
  wealth_quintile= "v190",
  wealth_idx= "v190a", # to check for wealth missing values
  Literacy= "v155",
  education_level= "v106", 
  source_water = "v113", # source of drinking water
  # person_id=WomenID,
  is_lactating= "v404", # breastfeeding yes=1, no=0
  is_smoker= "v463a" # Only cover cigarettes (other smoking variables)
) %>% dplyr::select(survey_cluster1, household_id1, LINENUMBER, survey_strata, PSU,
             region, urbanity,  age_year, wealth_quintile, wealth_idx, Literacy,
             education_level, source_water, is_lactating, is_smoker, sdist)


dim(DHSDATA)

# REVIEW: check "ultimate area unit" (v004)
str(DHSDATA)

# Checking class of discrete variables (imported from stata)

# Region
class(DHSDATA$region)
DHSDATA$region  <- as.factor(DHSDATA$region)

# Sex
#class(DHSDATA$sex)
#Malawi_WRA$sex  <- as.factor(DHSDATA$sex)

# Urbanity
class(DHSDATA$urbanity)
sum(is.na(DHSDATA$urbanity))
table(DHSDATA$urbanity)
DHSDATA$urbanity  <- as.factor(DHSDATA$urbanity)

# source of drinking water 
class(DHSDATA$source_water)
table(DHSDATA$source_water)
haven::print_labels(DHSDATA$source_water)
# New variable with descriptions
DHSDATA$source_water_ds <- haven::as_factor(DHSDATA$source_water) 

# Wealth quintile
class(DHSDATA$wealth_quintile)
sum(is.na(DHSDATA$wealth_quintile))
table(DHSDATA$wealth_quintile)
DHSDATA$wealth_quintile  <- as.factor(DHSDATA$wealth_quintile)

# Education
class(DHSDATA$education_level)
sum(is.na(DHSDATA$education_level))
table(DHSDATA$education_level)
haven::print_labels(DHSDATA$education_level)
DHSDATA$education_level  <- as.factor(DHSDATA$education_level)

# Literacy
class(DHSDATA$Literacy)
sum(is.na(DHSDATA$Literacy))
table(DHSDATA$Literacy)
haven::print_labels(DHSDATA$Literacy)
DHSDATA$Literacy  <- as.factor(DHSDATA$Literacy)

# Lactating
class(DHSDATA$is_lactating)
sum(is.na(DHSDATA$is_lactating))
table(DHSDATA$is_lactating)
haven::print_labels(DHSDATA$is_lactating) # 0=N, 1=Y
DHSDATA$is_lactating  <- as.factor(DHSDATA$is_lactating)

# Smoking 
# TODO: Checking smoking incidence in WRA in Malawi
class(DHSDATA$is_smoker)
sum(is.na(DHSDATA$is_smoker))
table(DHSDATA$is_smoker)
haven::print_labels(DHSDATA$is_smoker) # 0=N, 1=Y
DHSDATA$is_smoker  <- as.factor(DHSDATA$is_smoker)

# District
class(DHSDATA$sdist)
sum(is.na(DHSDATA$sdist))
table(DHSDATA$sdist)
haven::print_labels(DHSDATA$sdist) 

## District: New variable with district names ----
DHSDATA$dist_name <- haven::as_factor(DHSDATA$sdist) 
DHSDATA$sdist  <- as.factor(DHSDATA$sdist)


# Socio-economic data exploration:
# TODO: Would differences in WQ by region & residency
# impact our results? Probably yes. Check what that would mean for our analysis. 

# Wealth quintile 1-5 (lowest to highest)
# Education 0-3 (low to high)
hist(as.numeric(DHSDATA$wealth_quintile))
summary(DHSDATA$wealth_quintile)
plot(DHSDATA$wealth_quintile, DHSDATA$education_level, 
     main="Wealth vs Education",
     xlab="Wealth Q", ylab="Education level", pch=19)

# Literacy 0-2 (cant to can read) (3,4 others)
plot(DHSDATA$education_level, DHSDATA$Literacy,  
     main="Education vs Literacy",
     xlab="Education level", ylab="Literacy level", pch=19)

# Checking entries w/ no literacy but secondary educ. (n=36)
sum(as.numeric(DHSDATA$education_level) > 1 & DHSDATA$Literacy == 0)
DHSDATA$education_level[as.numeric(DHSDATA$education_level) >1 & DHSDATA$Literacy == 0]

# Region 1-3 (N->S)
plot( DHSDATA$region, DHSDATA$wealth_quintile,
     main="Wealth vs Region",
       xlab="Region", ylab="Wealth Q",pch=19)

plot(DHSDATA$region, DHSDATA$education_level,
     main="Education vs Region",
     xlab="Region", ylab="Education", pch=19)

# Highest Wealth Q in northern region
sum(as.numeric(DHSDATA$wealth_quintile) > 3 & DHSDATA$region == 1)/ sum(DHSDATA$region == 1) *100

DHSDATA %>% group_by (region) %>% dplyr::count(wealth_quintile)

# Merging DHS & MNS ----
# Merging with DHS dataset to obtain Wealth index and other variables 

EligibleDHS <- Malawi_WRA %>% 
 left_join(., DHSDATA %>% 
             dplyr::select(survey_cluster1, household_id1, LINENUMBER, 
                 wealth_quintile, wealth_idx, Literacy, education_level, 
            source_water, is_lactating, is_smoker, sdist, survey_strata,
            dist_name)) 

# Strata -----
EligibleDHS$survey_strata <-ifelse(
                  EligibleDHS$survey_cluster1==114,8,
           ifelse(EligibleDHS$survey_cluster1==140,1,
           ifelse(EligibleDHS$survey_cluster1==190,40, 
           ifelse(EligibleDHS$survey_cluster1==23,31, 
           ifelse(EligibleDHS$survey_cluster1==294,23, 
           ifelse(EligibleDHS$survey_cluster1==303,53,   
           ifelse(EligibleDHS$survey_cluster1==309,4,                                                                                
           ifelse(EligibleDHS$survey_cluster1==342,5,
           ifelse(EligibleDHS$survey_cluster1==37,10, 
           ifelse(EligibleDHS$survey_cluster1==377,29, 
           ifelse(EligibleDHS$survey_cluster1==410,33,
           ifelse(EligibleDHS$survey_cluster1==468,47,
           ifelse(EligibleDHS$survey_cluster1==5,25,  
           ifelse(EligibleDHS$survey_cluster1==511,27,
           ifelse(EligibleDHS$survey_cluster1==566,33, 
           ifelse(EligibleDHS$survey_cluster1==571,5,
           ifelse(EligibleDHS$survey_cluster1==601,29,
           ifelse(EligibleDHS$survey_cluster1==676,10, 
           ifelse(EligibleDHS$survey_cluster1==720,41,
                 EligibleDHS$survey_strata)))))))))))))))))))

# dhs_varibles  <- c("household_id1", "LINENUMBER",
# "region","wealth_quintile","Literacy","education_level", "is_lactating",
# "is_smoker","survey_strata","sdist")

# Selecting variables
#women  <- Malawi_WRA %>% dplyr::select(c(1:14, 21:23,
# 25, 27, 92, 96,97, 117, 131:162))  %>% names()

#Checking if intoducing duplicates
n01 == dim(EligibleDHS)[1] # need to be minus pregnant
n01 > dim(EligibleDHS)[1] # need to be minus pregnant
dim(Malawi_WRA)[1] == dim(EligibleDHS)[1] 

ddply(EligibleDHS, ~wealth_quintile, summarise, 
      medianBMI=matrixStats::weightedMedian(BMI, wt,na.rm = T))


# TODO: Sensitivity analysis for wealth and region, and for malaria prevalence.
# Data checks (non-weigheted)

#Cluster survey
unique(EligibleDHS$survey_cluster1)
sum(is.na(EligibleDHS$survey_cluster1)) #All observations have cluster
sum(EligibleDHS$survey_cluster1==0) #All observations have weight > 0

#household ID
unique(EligibleDHS$household_id1)
sum(is.na(EligibleDHS$household_id1)) #All observations have HH ID
sum(EligibleDHS$household_id1==0) #All observations have weight > 0

#Wealth Quantile vs Wealth idx (different for R/U)
unique(EligibleDHS$wealth_quintile)
sum(is.na(EligibleDHS$wealth_quintile)) # 29 observations are missing WQ
sum(is.na(EligibleDHS$wealth_idx)) # 29 observations are missing WQ
<<<<<<< HEAD:code/00_cleaning-dhs.R


=======


>>>>>>> 504882a58c86206e93b476f69f95a695e890ab42:00_cleaning-dhs.R
# Wealth Quintiles ----
boxplot(selenium ~ wealth_quintile, data = EligibleDHS, 
     main="Plasma Selenium by Wealth Quintile",
     xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19)

# Wealth Idx
boxplot(selenium ~ wealth_idx, data = EligibleDHS, 
        main="Plasma Selenium by Wealth Index",
        xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19)

boxplot(selenium ~ urbanity*wealth_quintile, data = EligibleDHS, 
        main="Plasma Selenium by Wealth Quintile & residency",
        xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

boxplot(selenium ~ urbanity*wealth_idx, data = EligibleDHS, 
     main="Plasma Selenium by Wealth Index & residency",
     xlab="Wealth Quantile", ylab="plasma Se (ng/ml)", pch=19, 
     col = c("white", "steelblue"), frame = FALSE)

boxplot(BMI ~ urbanity*wealth_quintile, data = EligibleDHS, 
     main="BMI by Wealth Quintile & residency",
     xlab="Wealth Quantile", ylab="BMI (kg/m^2)", pch=19, 
     col = c("white", "steelblue"), frame = FALSE)

boxplot(BMI ~ urbanity*wealth_idx, data = EligibleDHS, 
        main="BMI by Wealth Quintile & residency",
        xlab="Wealth Quantile", ylab="BMI (kg/m^2)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

boxplot(selenium ~ urbanity*region, data = EligibleDHS, 
        main="Plasma Selenium by region & residency",
        xlab="Region", ylab="plasma Se (ng/ml)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

boxplot(BMI ~ urbanity*region, data = EligibleDHS, 
        main="BMI by Wealth Quintile & residency",
        xlab="Region", ylab="BMI (kg/m^2)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

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
sum(is.na(EligibleDHS$Literacy)) # 29 observations same as missing from the DHS survey file  

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

## District --------
unique(EligibleDHS$sdist)
sum(is.na(EligibleDHS$sdist)) #29 observations are missing
table(EligibleDHS$sdist)

## Fixing the missing values using cluster (EA) info (see documentation)
# Checking if we have same cluster with sdist for imputing.
test <- unique(EligibleDHS$survey_cluster1[is.na(EligibleDHS$sdist)])

EligibleDHS %>% filter(survey_cluster1 %in% test) %>% 
  distinct(survey_cluster1, sdist)

# Fixing the missing values
for(i in 1:length(test)){
  
  x <- unique(na.omit(EligibleDHS$sdist[EligibleDHS$survey_cluster1 %in% test[i]]))
  EligibleDHS$sdist[EligibleDHS$survey_cluster1 %in% test[i]] <- x
  
  x <- unique(na.omit(EligibleDHS$dist_name[EligibleDHS$survey_cluster1 %in% test[i]]))
  EligibleDHS$dist_name[EligibleDHS$survey_cluster1 %in% test[i]] <- x
}

sum(is.na(EligibleDHS$sdist))

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

## Inputting NA for WQ info ----

#Identifying HH ID & cluster of missing socio-eco info
subset(EligibleDHS, is.na(EligibleDHS$wealth_quintile), 
       select = c("household_id1","survey_cluster1")) %>% left_join(.,DHSDATA) %>% 
  group_by(survey_cluster1, household_id1) %>% dplyr::count(wealth_quintile, wealth_idx, urbanity) %>% 
  filter(!is.na(wealth_quintile)) %>% 
  View()

#Sorting Wealth Q for each HH ID & cluster of missing info
Wealth <- subset(EligibleDHS, is.na(EligibleDHS$wealth_quintile), 
       select = c("household_id1","survey_cluster1")) %>% 
   left_join(., DHSDATA %>% 
             dplyr::select(c("household_id1","survey_cluster1", "wealth_quintile", "wealth_idx"))) %>% 
  group_by(survey_cluster1, household_id1) %>%
  filter(!is.na(wealth_quintile)) %>% 
  distinct()

#Checking rows with missing value for wealth Q
n <- which(is.na(EligibleDHS$wealth_quintile))
#Checking rows with missing value for wealth Indx
m <- which(is.na(EligibleDHS$wealth_idx))
# Checkin that are the same 
table(n==m)

# Checking line id (hh member id) with & w/o missing value for wealth Q w/ same 
# H ID & cluster ID as in wealth df
EligibleDHS$LINENUMBER[#is.na(EligibleDHS$wealth_quintile) &
  EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[1] &
                              EligibleDHS$household_id1 %in% Wealth$household_id1[1]]

#Generate a loop to assign Wealth Q from other member of the HH to non-reported people in biomarker survey data
for(i in seq_along(Wealth$household_id1)){
  
EligibleDHS$wealth_quintile[EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[i] &
                              EligibleDHS$household_id1 %in% Wealth$household_id1[i]] <- Wealth$wealth_quintile[i]

EligibleDHS$wealth_idx[EligibleDHS$survey_cluster1 %in% Wealth$survey_cluster1[i] &
                              EligibleDHS$household_id1 %in% Wealth$household_id1[i]] <- Wealth$wealth_idx[i]
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

#subset(EligibleDHS, !is.na(selenium), 
#select = c(v005, survey_weight))

## Malaria vs wealth & urbanity
plot(EligibleDHS$wealth_quintile, EligibleDHS$Malaria_test_result)
plot(EligibleDHS$urbanity, EligibleDHS$Malaria_test_result)

boxplot(selenium ~ Malaria_test_result*urbanity, data = EligibleDHS, 
        main="Plasma Se by Residency & Malaria test",
        xlab="Residency", ylab="plasma Se (ng/ml)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

boxplot(selenium ~ Malaria_test_result*wealth_quintile, data = EligibleDHS, 
        main="Plasma Se by Wealth Q & Malaria test",
        xlab="Wealth Q", ylab="plasma Se (ng/ml)", pch=19, 
        col = c("white", "steelblue"), frame = FALSE)

#Source of water ----

# Checking median plasma Se by water source
ddply(EligibleDHS, ~source_water, summarise, 
      medianBMI=matrixStats::weightedMedian(selenium, wt,na.rm = T))

# EligibleDHS$survey_cluster1[EligibleDHS$source_water == "96"]

# Defining Se deficiency ----
EligibleDHS$LOW_SEL_GPx3 <- ifelse(EligibleDHS$selenium<84.9,1,0)
EligibleDHS$LOW_SEL_IDI <- ifelse(EligibleDHS$selenium<64.8,1,0)
EligibleDHS$LOW_SEL_KD <- ifelse(EligibleDHS$selenium<30,1,0)

# Checking the dimension of the dataset
dim(EligibleDHS)

# Saving Se dataset into R object
# saveRDS(EligibleDHS,  file=here::here("data", "inter-output","dhs_se.rds"))

# EligibleDHS  <- readRDS(here::here("data","inter-output","dhs_se.rds")) 

# GPS Location  ----
# Loading the dataset
GPS <- st_read(here::here("data", "MWGE7AFL", "MWGE7AFL.shp")) # GPS location DHS
names(GPS)

# Renaming variables
GPS <- dplyr::rename(GPS, survey_cluster1='DHSCLUST', Latitude='LATNUM', 
                    Longitude='LONGNUM',  altitude_in_metres='ALT_GPS')
dim(GPS)

#  Generating the offset buffer

for(i in 1:nrow(GPS)){

# Assigning buffer size (in m) acc. to Urban (U) or Rural (R)
offset.dist<-ifelse(GPS$URBAN_RURA[i]=="U", 2000, 5000)

# Generating the buffers around the centroids
GPS$buffer[i] <- st_buffer(GPS$geometry[i], dist = offset.dist)

}

# Transforming the list into spatial class
GPS <- GPS %>% st_drop_geometry() 
GPS$survey_cluster1 <- as.character(GPS$survey_cluster1)

GPS <- GPS %>% left_join(., ea_admin %>% 
                           select(survey_cluster1, ADM2_EN))

GPS$buffer <- st_as_sfc(GPS$buffer)

# Checking that the output
plot(GPS$buffer) # Plotting the buffer
plot(GPS$buffer[GPS$URBAN_RURA == "U"],col='red',add=TRUE) # colouring red those that are urban (smaller radius)

# Merging with the dataset
dim(EligibleDHS)
dim(GPS)
EligibleDHS <- left_join(EligibleDHS, GPS) 
dim(EligibleDHS)

# Only for Se in the dataset
# GPS_Se <- merge(EligibleDHS[, c("unique_id", "survey_cluster1", "sdist",  "selenium", "wealth_quintile", "region")], GPS, by='survey_cluster1')
# GPS_Se  <- st_as_sf(EligibleDHS[, c("unique_id", "survey_cluster1", "sdist", "urbanity",  "selenium", "survey_weight",
 # "wealth_quintile", "BMI", "" "region")], crs = st_crs(4326), coords = c('LONGNUM', 'LATNUM'))
 
# Maps-testing: Visualising Se conc.
EligibleDHS  %>% 
 st_as_sf(., coords = c('Longitude', 'Latitude'))  %>% 
ggplot() + 
  geom_sf(aes(color = selenium))

# tm_shape(b_admin1) +
#   tm_polygons() +
#   tm_shape(GPS_Se) + 
#   tm_symbols(col = "black", size = "selenium")

#boundaries$shapeID[boundaries$shapeID == "60268647B1308848342151"] 
#  boundaries  %>% filter(shapeID != "60268647B1308848342151")  %>% 
#  tm_shape() +
#  tm_polygons() +
#  tm_shape(dhs_se) + 
#  tm_symbols(col = "black", size = "selenium")
 # Saving Se dataset into R object
#saveRDS(GPS_Se, file=here::here("data", "inter-output","dhs_se.rds"))

<<<<<<< HEAD:code/00_cleaning-dhs.R
# Saving DHS + GPS dataset into R object
saveRDS(EligibleDHS, file=here::here("data", "inter-output","dhs_se_gps.rds"))

## Checking BMI per district ======
EligibleDHS %>% ggplot(aes(reorder(dist_name, BMI, FUN = median), BMI)) +
  geom_boxplot() + 
  geom_hline(aes(yintercept = median(BMI, na.rm = TRUE)), colour = "blue") +
  geom_hline(aes(yintercept = 18), colour = "red") +
  facet_wrap(~region, scales = "free_x") 
  

# Survey analysis: Applying survey weight ----
EligibleDHS  <- readRDS(file=here::here("data", "inter-output","dhs_se_gps.rds"))
#EligibleDHS  <- readRDS(file=here::here("data", "inter-output","dhs_se.rds"))
sum(is.na(EligibleDHS$selenium))
sum(is.na(EligibleDHS$WEIGHT))
EligibleDHS  <- subset(EligibleDHS, !is.na(selenium))
EligibleDHS  <- subset(EligibleDHS, !is.na(WEIGHT))

class(EligibleDHS$urbanity)

# EligibleDHS$survey_cluster1  <- as.factor(EligibleDHS$survey_cluster1)
# EligibleDHS$wealth_quintile  <- as.factor(haven::zap_labels(EligibleDHS$wealth_quintile))
# EligibleDHS$sdist <- haven::zap_labels(EligibleDHS$sdist)
# EligibleDHS$region  <- haven::zap_labels(EligibleDHS$region)

#EligibleDHS$wealth_quintile <- factor(EligibleDHS$wealth_quintile,
#    levels=c (1,2,3,4,5), 
#                    labels=c('Lowest Wealth Q','Low Wealth Q',
#                     'Middle Wealth Q', "High Wealth Q", "Highest Wealth Q"), ordered=TRUE)


table(EligibleDHS$urbanity)
#table(EligibleDHS$URBAN_RURA)
# table(EligibleDHS$had_malaria)
table(EligibleDHS$Malaria_test_result)
# table(EligibleDHS$ANY_INFLAMMATION)

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


=======
dim(EligibleDHS)
>>>>>>> 504882a58c86206e93b476f69f95a695e890ab42:code/00_cleaning-dhs.R
=======
dim(EligibleDHS)
>>>>>>> 504882a58c86206e93b476f69f95a695e890ab42:00_cleaning-dhs.R

# 3) Saving DHS + GPS dataset into R object ----
# saveRDS(EligibleDHS, file=here::here("data", "inter-output","dhs_se_gps.rds"))
