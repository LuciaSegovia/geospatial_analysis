
#######################################################################
#
#   Obtaining maize Se conc. dataset for different level of aggregation
#     (Predicted-mean of Se conc.)
#
#####################################################################################################

# Cleaning the environment
rm(list = ls())

# Loading libraries and functions
#install.packages("rgeos")

library(dplyr) # data wrangling
#library(plyr) # weighted data analysis
library(ggplot2) # visualisation
#library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("CEPHaStat_3.R")) #stat functions
library(geoR)  # geospatial modelling
#library(maps)
#library(mapdata) 
library(nlme)  # linear mixed model
library(Matrix)
library(numDeriv)

########################################################################

##################################################################################

#    Maize Se conc. 

############################################################################################################## 


# Loading the maize Se conc. dataset (cleaned from 01.cleaning-location.R)
data.df  <- readRDS(here::here("data", "inter-output",
"mwi-predicted-maizeSe_LOD_admin.RDS")) # cleaned geo-loc maize Se data (00_cleaning-location.R)

names(data.df)
# Checking missing values: 2 pH
sum(is.na(data.df$BIO1))
data.df[which(is.na(data.df$pH_w)),]
data.df  <- subset(data.df, !is.na(pH_w)) # removing NA

#sum(duplicated(dhs_se$unique_id))
#length(unique(dhs_se$survey_cluster1))
#class(dhs_se$wealth_quintile)
#class(dhs_se$sdist)
#dhs_se$wealth_quintile  <- haven::zap_labels(dhs_se$wealth_quintile)

#plot(dhs_se[, "wealth_quintile"])
#table(dhs_se$wealth_quintile, dhs_se$region)

# Selecting the Se variable to model
var  <- "Se_grain"

# Checking no. of EAs per (Se_grain (1123) vs pred.Se (1628))
length(unique(data.df$EACODE[!is.na(data.df[, var])]))

# check for normality
summaplot(data.df[, var])
summary(data.df[, var])
sum(is.na(data.df[, var]))
sum(is.na(data.df$pH_w))
data.df$logSe<-log(data.df[, var])
summaplot(data.df$logSe)

par(mfrow=c(1,2))
 plot(log(data.df$Se_grain), data.df$pH_w)
 plot(log(data.df$Se_grain), data.df$BIO1)

# visualize the data
ggplot(data = data.df,
       mapping = aes(x = logSe, y = BIO1/10)) + 
  geom_point() +
  facet_wrap(~DISTRICT, ncol = 5) +
  labs(x = "Se conc. in maize (log(mg/kg))", 
       y = "Downscaled MAT") + 
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12))

#data.df  <- data.df[,-c(9, 10)]
#data.df$BIO1b  <- data.df$BIO1/10

# fit the model: Maize Se
model0 <- lme(logSe~1, random=~1|EACODE, data=data.df, method = "ML")

model1 <-lme(logSe ~ pH + BIO1, random=~1|EACODE, data=data.df, method = "ML")

# model1b<-lme(logSe ~ BIO1, random=~1|EACODE, data=data.df, method = "ML")

anova(model0, model1)

# Model 1: with covariates is performing better: Keeping cov.

# check distribution of residuals
summaplot(residuals(model1,level=0))

model2<-lme(logSe ~ pH + BIO1, random=~1|TA_CODE, data=data.df, method = "ML")

model3<-lme(logSe ~ pH + BIO1, random=~1|TA_CODE/EACODE, data=data.df, method = "ML")

model3b<-lme(logSe ~1, random=~1|TA_CODE/EACODE, data=data.df, method = "ML")

anova(model2, model3, model3b)

# Model 3: w/ nested random effect is better

# check distribution of residuals
summaplot(residuals(model3,level=0))

model4<-lme(logSe ~ pH + BIO1, random=~1|DISTRICT, data=data.df, method = "ML")

model5<-lme(logSe ~ pH + BIO1, random=~1|DISTRICT/TA_CODE, data=data.df, method = "ML")

model6<-lme(logSe ~ pH + BIO1, random=~1|DISTRICT/TA_CODE/EACODE, data=data.df, method = "ML")

anova(model4, model5, model6)

# Model 6: w/ full nested random effect is better

# check distribution of residuals
summaplot(residuals(model6,level=0))


## Getting the resutls of the model prediction at diff aggregation/area

# output the results (model 1)
area  <- "EACODE"

summary(model1)
fixef(model1) # fixed effects
n  <- fixef(model1)[1] # fixed effects (intercept) (-7.768999)
pH_b  <- fixef(model1)[2] # beta (pH) (0.1940353)
bio1_b  <- fixef(model1)[3] # beta (BIO1) (0.01346473)
re <- ranef(model1) # random effects (log Se mean per EA)
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- area
names(re)[2]  <- "intercept"
head(re)

# Calculating the covariate means for predictions
re_cov  <- data.df  %>% group_by(!!sym(area))  %>% 
summarise(pH_mean = mean(pH), 
          BIO1_mean = mean(BIO1))

# Merging mean of the covariates w/ model results
re  <- merge(re, re_cov)

# Calculating the predicted-mean Se
re$maizeSe_mean <- exp((re$intercept+n)+(pH_b*re$pH_mean)+(bio1_b*re$BIO1_mean))


head(re)
hist(re$maizeSe_mean)
summary(re$maizeSe_mean)

# Dataset for modelling
ea.df  <- re
ea.df$admin  <- area
names(ea.df)[names(ea.df) == area]  <- "admin_id"

head(ea.df)

#saveRDS(ea.df, here::here("data", "inter-output", "maizeSe-mean-EA.RDS"))

# output the results (model 3)
area  <- "TA_CODE"

summary(model3)
fixef(model3) # fixed effects
n  <- fixef(model3)[1] # fixed effects (intercept) (-7.95739323)
pH_b  <- fixef(model3)[2] # beta (pH) (0.0.20074824)
bio1_b  <- fixef(model3)[3] # beta (BIO1) (0.01418672)
re <- ranef(model3)[[1]] # random effects (log Se mean per TA)
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- area
names(re)[2]  <- "intercept"

# Calculating the covariate means for predictions
re_cov  <- data.df  %>% group_by(!!sym(area))  %>% 
summarise(pH_mean = mean(pH), 
          BIO1_mean = mean(BIO1))

# Merging mean of the covariates w/ model results
re  <- merge(re, re_cov)

# Calculating the predicted-mean Se
re$maizeSe_mean  <- exp((re$intercept+n)+(pH_b*re$pH_mean)+(bio1_b*re$BIO1_mean))


head(re)
hist(re$maizeSe_mean )
summary(re$maizeSe_mean )

#Checking that all the TA in the data are there
length(unique(re$admin_id))
length(unique(data.df$TA_CODE))

# Dataset for modelling
ta.df  <- re
ta.df$admin  <- area
names(ta.df)[names(ta.df) == area]  <- "admin_id"

# output the results (model 6)
area  <- "DISTRICT"

summary(model6)
fixef(model6) # fixed effects
n  <- fixef(model6)[1] # fixed effects (intercept) (-7.95956540)
pH_b  <- fixef(model6)[2] # beta (pH) (0.19218561)
bio1_b  <- fixef(model6)[3] # beta (BIO1) (0.01449493)
re <- ranef(model6)[[1]] # random effects (log Se mean per Dist)
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- area
names(re)[2]  <- "intercept"

# Calculating the covariate means for predictions
re_cov  <- data.df  %>% group_by(!!sym(area))  %>% 
summarise(pH_mean = mean(pH), 
          BIO1_mean = mean(BIO1))

# Merging mean of the covariates w/ model results
re  <- merge(re, re_cov)

# Calculating the predicted-mean Se
re$maizeSe_mean  <- exp((re$intercept+n)+(pH_b*re$pH_mean)+(bio1_b*re$BIO1_mean))

head(re)
hist(re$maizeSe_mean )
summary(re$maizeSe_mean )

length(unique(re$DISTRICT))
length(unique(data.df$DISTRICT))

dist.df  <- re
dist.df$admin  <-  area
names(dist.df)[names(dist.df) == area]  <- "admin_id"

data.df  <-  do.call(rbind, list(ea.df, ta.df, dist.df))

saveRDS(data.df, here::here("data", "inter-output", "maizeSe-mean-predicted.RDS"))

##################################################################################

#    Plasma Se conc. 

############################################################################################################## 


# Loading the plasma Se conc. dataset (cleaned from 01.cleaning-location.R)
data.df  <- readRDS(here::here("data", "inter-output","mwi-plasma-se_admin.RDS")) # cleaned geo-loc maize Se data


# Checking BMI & wealth Q (co-linearity)
EligibleDHS$wealth_quintile  <- haven::zap_labels(EligibleDHS$wealth_quintile)
plot(EligibleDHS$selenium, EligibleDHS$BMI, col=EligibleDHS$wealth_quintile)
ggplot(EligibleDHS, aes(BMI, selenium, col=wealth_quintile)) + geom_point()

plot(EligibleDHS$wealth_quintile, EligibleDHS$BMI)
ggplot(EligibleDHS, aes(BMI, wealth_quintile, col=selenium)) + geom_point()


# Checking missing values: 34 selenium, 13 wealth_quintile
sum(is.na(data.df$selenium))
sum(is.na(data.df$wealth_quintile))
sum(is.na(data.df$BMI))
data.df[which(is.na(data.df$wealth_quintile)),]
data.df  <- subset(data.df, !is.na(selenium) &
 !is.na(wealth_quintile)) # removing NA

dim(data.df)
# check for normality
summaplot(data.df$selenium)
summary(data.df$selenium)
sum(is.na(data.df$selenium))
sum(is.na(data.df$wealth_quintile))
data.df$logSe<-log(data.df$selenium)
summaplot(data.df$logSe)
summary(data.df$selenium)

# visualize the data - Checking variability w/i EA by region
data.df  %>%  dplyr::filter(ADM1DHS == "3")  %>% 
ggplot() + 
  geom_boxplot(mapping = aes( y = selenium)) +
  facet_wrap(~EACODE, ncol = 5) +
  labs(#x = "Enumeration Area" , 
       y = "Se conc. in palsma (nmol/mL)") + 
       geom_hline(yintercept =mean(data.df$selenium), colour = "red")+
  scale_x_continuous(breaks = 0:4 * 2) +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# There are three EAs in central reagion w/ "super high" Se checking if in Salima (YES) & Ntchisi
# Central c("20501019", "20502023", "20304005")
# The other EAs high-ish in the south were located in the districts of Chikwawa & Nsanje

 data.df  %>% filter(EACODE %in% c("31002006", "31101010", "31106006", "31120704"))  %>% 
 select(DISTRICT, EACODE, wealth_quintile)

data.df  %>% group_by(EACODE)  %>% 
summarise(variance = var(logSe))  %>% 
arrange(desc(variance))  %>% plot()

# fit the model: Plasma Se
model0<-lme(logSe~1, random=~1|EACODE, data=data.df, method = "ML")

model1<-lme(logSe ~ wealth_quintile, random=~1|EACODE, data=data.df, method = "ML")

anova(model0, model1)

# check distribution of residuals
summaplot(residuals(model0,level=0))

model2<-lme(logSe ~1, random=~1|DISTRICT, data=data.df, method = "ML")

model3<-lme(logSe ~1, random=~1|DISTRICT/EACODE, data=data.df, method = "ML")

model3b<-lme(logSe ~ wealth_quintile, random=~1|DISTRICT/EACODE, data=data.df, method = "ML")


anova(model2, model3, model3b)

anova(model0, model2, model3)

summary(model0)
summary(model3)


class(re$EACODE)
admin$EACODE  <- as.character(admin$EACODE)

re %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#plot()
ggplot() + 
  geom_sf(aes(fill = se_mean)) 

Se_admin  %>% st_drop_geometry()  %>% 
# group_by(across(all_of(name_var))) %>% 
 dplyr::group_by(NAME_1)  %>% 
dplyr::summarise(Se_median = median(selenium, na.rm = T))   %>% 
full_join(., admin)  %>% st_as_sf()  %>% 
#.[1,]  %>% 
ggplot() + 
  geom_sf(aes(fill = Se_median)) 
 
re_dist  <- re
re_ea  <- re

names(re_dist)[1]  <- "intercept"
re_dist$se_mean  <- exp(re_dist$intercept+n)
re_dist<- tibble::rownames_to_column(re_dist, var ="NAME_1")
head(re_dist)

write(re, "re_ea.csv")
write(re_dist, "re_ea_district.csv")

# Saving outputs of the model for plasma and maize
plasma_se  <- re
maize_se  <- re


#Joining them, so we get the adim areas with corresponding data
test  <- right_join(maize_se,  plasma_se, by = "ID_2")

#x - independent variable (explanatory) - maize Se
#y - dependent variable (response) - plasma Se
x  <-  test$se_mean.x  # maize Se
y  <-  test$se_mean.y  # plasma Se

plot(x, y)
abline(lm(y ~ x, data = test), col = "red")
