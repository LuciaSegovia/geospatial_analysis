# Loading libraries and functions

library(dplyr) # data wrangling 
library(plyr) # weighted data analysis
library(ggplot2) # visualisation
library(survey) # survey design
library(sf) #spatial data manipulation
library(tmap)  #spatial data manipulation and visualisation
source(here::here("CEPHaStat_3.R")) #stat functions

# Loading the datat
#check dhs.R script
dhs.df <- readRDS(here::here("data", "inter-output", "dhs.rds")) # cleaned dhs data
dhs_se  <- readRDS(here::here("data","inter-output","dhs_se.rds")) # Spatial plasma Se data

#check maizeSe.R script
maize_se  <- readRDS(here::here("data", "inter-output","maize_se.rds")) # cleaned Spatial maize Se data


# Allocating Se values to each admin unit
# Choose the dataset:
Se.df  <- maize_se 
# Se.df  <- dhs_se 
Se_admin = st_intersection(Se.df, admin)

names(Se_admin)
dim(Se_admin)
subset(Se_admin, NAME_1 == "Balaka") 

# Checking plasma values for the model
#Rename your variable:
names(Se_admin)
names(Se_admin)[1]  <- "selenium"
# check for normality
summaplot(Se_admin$selenium)
sum(is.na(Se_admin$selenium))
Se_admin  <- subset(Se_admin, !is.na(selenium)) # removing NA
Se_admin$sel_log<-log(Se_admin$selenium)
summaplot(Se_admin$sel_log)

# fit the model: Plasma Se
model<-lme(sel_log~1, random=~1|ID_3, data=Se_admin)

# check distribution of residuals
histplot(residuals(model,level=0))
summaplot(residuals(model,level=0))

# output the results
summary(model)
fixef(model) # fixed effects
n  <- fixef(model) # fixed effects
re <- ranef(model) # random effects

names(re)[1]  <- "intercept"
re$se_mean  <- exp(re$intercept+n)

re <- tibble::rownames_to_column(re, var = paste0("ID_", bn))
head(re)
hist(re$se_mean)


# Saving outputs of the model for plasma and maize
plasma_se  <- re
maize_se  <- re

#Joining them, so we get the adimn areas with corresponding data
test  <- left_join(plasma_se, maize_se, by = "ID_3")

#x - independent variable (explanatory) - maize Se
#y - dependent variable (response) - plasma Se
x  <- test$se_mean.y
y  <-  test$se_mean.x

plot(x, y)
abline(lm(y ~ x, data = test), col = "red")
