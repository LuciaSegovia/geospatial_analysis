
#######################################################################
#
#   Obtaining maize Se conc. dataset for different level of aggregation
#     (Predicted-mean of Se conc.)
#
#####################################################################################################

# Loading libraries and functions

library(dplyr) # data wrangling 
#library(plyr) # weighted data analysis
#library(ggplot2) # visualisation
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


# Loading the maize Se conc. dataset (cleaned from 01.cleaning-location.R)
data.df  <- readRDS(here::here("data", "inter-output","mwi-maize-se_admin.RDS")) # cleaned geo-loc maize Se data

# Checking missing values: 2 pH
sum(is.na(data.df$BIO1))
data.df[which(is.na(data.df$pH)),]
data.df  <- subset(data.df, !is.na(pH)) # removing NA


#sum(duplicated(dhs_se$unique_id))
#length(unique(dhs_se$survey_cluster1))
#class(dhs_se$wealth_quintile)
#class(dhs_se$sdist)
#dhs_se$wealth_quintile  <- haven::zap_labels(dhs_se$wealth_quintile)

#plot(dhs_se[, "wealth_quintile"])
#table(dhs_se$wealth_quintile, dhs_se$region)


# Checking plasma values for the model
#Rename your variable:
#names(Se_admin)
#names(Se_admin)[3]  <- "sdist" 
#names(Se_admin)[4]  <- "selenium" 


# check for normality
summaplot(data.df$Se_mg)
summary(data.df$Se_mg)
sum(is.na(data.df$Se_mg))
sum(is.na(data.df$pH))
data.df$logSe<-log(data.df$Se_mg)
summaplot(data.df$logSe)


#REVIEW: Values of the final model, seemed quite low and high influenced by around the mean values

#data.df  <- data.df[,-c(9, 10)]

# fit the model: Maize Se
model0<-lme(logSe~1, random=~1|EACODE, data=data.df, method = "ML")

model1<-lme(logSe ~ pH + BIO1, random=~1|EACODE, data=data.df, method = "ML")

anova(model0, model1)

# Model 1: with covariates is performing better: Keeping cov.

# check distribution of residuals
summaplot(residuals(model1,level=0))

model2<-lme(logSe ~ pH + BIO1, random=~1|TA_CODE, data=data.df, method = "ML")

model3<-lme(logSe ~ pH + BIO1, random=~1|TA_CODE/EACODE, data=data.df, method = "ML")

model3<-lme(logSe ~1, random=~1|TA_CODE/EACODE, data=data.df, method = "ML")

anova(model2, model3)

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

#model1  <- model0
# output the results (model 1)
summary(model1)
fixef(model1) # fixed effects
n  <- fixef(model1)[1] # fixed effects (intercept) (-7.768999)
re <- ranef(model1) # random effects (log Se mean per EA)

# output for nested outcome
#re  <- re$NAME_1
#re  <- re$ID_3
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- "EACODE"
names(re)[2]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)

head(re)
hist(re$se_mean)
summary(re$se_mean)
ea.df  <- re


# output the results (model 3)
summary(model3)
fixef(model3) # fixed effects
n  <- fixef(model3)[1] # fixed effects (intercept) (-7.95739323)
re <- ranef(model3)[[1]] # random effects (log Se mean per TA)
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- "TA_CODE"
names(re)[2]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)

head(re)
hist(re$se_mean)
summary(re$se_mean)

#Checking that all the TA in the data are there
length(unique(re$TA_CODE))
length(unique(data.df$TA_CODE))

ta.df  <- re

# output the results (model 6)
summary(model6)
fixef(model6) # fixed effects
n  <- fixef(model6)[1] # fixed effects (intercept) (-7.95956540)
re <- ranef(model6)[[1]] # random effects (log Se mean per Dist)
re <- tibble::rownames_to_column(re)
names(re)
names(re)[1]  <- "DISTRICT"
names(re)[2]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)

head(re)
hist(re$se_mean)
summary(re$se_mean)

length(unique(re$DISTRICT))
length(unique(data.df$DISTRICT))

dist.df  <- re



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
