
## This scripts provide some anlysis and results of the Plasma Se concentration
## from the 2015-16 MDHS-MNS 

# Loading libraries
library(plyr) # weighted data analysis
library(dplyr) # data wrangling 
library(ggplot2) # visualisation
library(survey) # survey design
library(srvyr) # survey design 2
#options(survey.lonely.psu="adjust") # For Error of one PSU at stage 1

# Loading data
## DHS data
EligibleDHS  <- readRDS(file=here::here("data", "inter-output","dhs_se_gps.rds"))


# Survey analysis: Applying survey weight ----

names(EligibleDHS)
sum(is.na(EligibleDHS$selenium))
sum(is.na(EligibleDHS$AGE_IN_YEARS))
sum(is.na(EligibleDHS$wealth_idx))
sum(is.na(EligibleDHS$agp))
sum(is.na(EligibleDHS$crp))
sum(is.na(EligibleDHS$urbanity))
sum(is.na(EligibleDHS$Longitude))
sum(is.na(EligibleDHS$Latitude))
sum(is.na(EligibleDHS$survey_cluster1))
class(EligibleDHS$wealth_idx)
EligibleDHS$wealth_idx <- as.factor(EligibleDHS$wealth_idx)
#EligibleDHS1  <- subset(EligibleDHS, !is.na(selenium))
#EligibleDHS2  <- subset(EligibleDHS1, !is.na(wealth_idx))
#EligibleDHS3  <- subset(EligibleDHS2, !is.na(agp))
EligibleDHS  <- subset(EligibleDHS, !is.na(selenium) & 
                         !is.na(wealth_idx) & !is.na(agp))

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
# PSU = cluster
DHSdesign<-svydesign(id=~survey_cluster1,  weights=~survey_weight,
                     strata=~urbanity+region, #This strata
                     data=subset(EligibleDHS, !is.na(selenium)))

DHSdesign2 <- EligibleDHS %>%
  as_survey_design(
    weights = survey_weight,
    strata = c(urbanity, region),
    ids = survey_cluster1,
    nest = TRUE
  )

# Checking the distribution of all variables reported in Table 2. 
# Plasma Se
svyhist(~selenium,   DHSdesign) # Median
svyhist(~crp,   DHSdesign) # Median
svyhist(~agp,   DHSdesign) # Median
svyhist(~AGE_IN_YEARS,   DHSdesign) # Median

# Median 
variables <- c("selenium", "crp", "agp", "AGE_IN_YEARS")

## Table 2 -----
DHSdesign2 %>% group_by(sdist) %>% 
  summarise(Se = survey_quantile(selenium,
                                 quantiles = c(0.25, .5, 0.75)),
            crp = survey_quantile(crp,
                                  quantiles = c(0.25, .5, 0.75)), 
            agp = survey_quantile(agp,
                                  quantiles = c(0.25, .5, 0.75)),                                 
            Age = survey_quantile(AGE_IN_YEARS,
                                  quantiles = c(0.25, .5, 0.75))) %>% 
  select(-ends_with("se")) %>% 
  arrange(desc(Se_q50))

# Complex sample design parameters
# PSU = cluster
# DHSdesign2<-svydesign(id=~survey_cluster1+household_id1,  weights=~survey_weight,
# strata=~urbanity+region, #This strata
#  data=subset(EligibleDHS, !is.na(selenium)))
# 
# We are not going to use this strata bc is from the dhs which is different 
# eg district level representatives 
#options(survey.lonely.psu="adjust", survey.ultimate.cluster = TRUE)
#DHSdesign2<-svydesign(id=~survey_cluster1,  weights=~survey_weight,
#                    strata=~survey_strata, #This strata
#                   data=EligibleDHS)
## Se deficiency estimates ----
svymean(~LOW_SEL_GPx3,DHSdesign)
# tabulate indicator by region
svyby(~selenium, ~wealth_idx,  DHSdesign, svymean, vartype=c("se","ci"), na.rm = TRUE)
svyby(~selenium, ~urbanity,  DHSdesign, svymean, vartype=c("se","ci"), na.rm = TRUE)
svyby(~LOW_SEL_GPx3, ~urbanity,  DHSdesign, svymean, vartype=c("se","ci"), na.rm = TRUE)
svyby(~LOW_SEL_GPx3, ~region,  DHSdesign, svymean, vartype=c("se","ci"), na.rm = TRUE)
#svyby(~LOW_SEL_GPx3, ~sdist,  DHSdesign, svymean, vartype=c("se","ci"), na.rm = TRUE)
svyby(~selenium, ~urbanity*wealth_quintile,  DHSdesign, svymean, vartype=c("se","ci"))

svyhist(~log(BMI),   DHSdesign)
svyhist(~BMI,   DHSdesign)

svyboxplot(selenium~urbanity,   DHSdesign)
svyboxplot(selenium~wealth_quintile,   DHSdesign)
svyboxplot(selenium~sdist,   DHSdesign)
svyboxplot(selenium~region,   DHSdesign)
svyboxplot(AGE_IN_YEARS~region,   DHSdesign)

# selenium, AGE_IN_YEARS,agp, crp
round(svymean(~crp, DHSdesign, na.rm = TRUE), 2)
svyquantile(~crp, DHSdesign, c(.25,.5,.75), na.rm = TRUE)

# T test
survey::svyttest(log(selenium)~urbanity, DHSdesign)
survey::svyttest(LOW_SEL_GPx3~urbanity, DHSdesign)

# X^2 
svychisq(~wealth_idx+urbanity, DHSdesign) # No diff. bc it's rural/urban adjusted
svychisq(~wealth_quintile+urbanity, DHSdesign) # Diff. because it's at national-level

# anova

test <- svyby(~AGE_IN_YEARS,~BMI_cat, DHSdesign, svymean, covmat=TRUE, na.rm = T) 

svychisq(~AGE_IN_YEARS+urbanity, DHSdesign)

prop.table(svytable(~urbanity, DHSdesign))*100
prop.table(svytable(~wealth_idx, DHSdesign))*100

svyciprop(~factor(wealth_idx), DHSdesign)

# Checking the model
m0 <- svyglm(log(selenium)~region,design=DHSdesign, family=gaussian())
m0 <- svyglm(log(selenium)~wealth_idx,design=DHSdesign, family=gaussian())
#m1 <- svyglm(log(selenium)~wealth_quintile,design=DHSdesign, family=gaussian())
m1 <- svyglm(log(selenium)~urbanity,design=DHSdesign, family=gaussian())
m2 <- svyglm(log(selenium)~urbanity+region,design=DHSdesign, family=gaussian())

summary(m1)

AIC(m0, m1, m2)


# Testing models
m0 <- svyglm(selenium~wealth_idx,design=DHSdesign, family=gaussian())
m1 <- svyglm(selenium~wealth_idx+urbanity,design=DHSdesign, family=gaussian())
m2 <- svyglm(selenium~wealth_idx+urbanity+AGE_IN_YEARS,design=DHSdesign, family=gaussian())
m3 <- svyglm(selenium~AGE_IN_YEARS,design=DHSdesign)
m4 <- svyglm(selenium~wealth_quintile,design=DHSdesign, family=gaussian())
m5 <- svyglm(log(selenium)~wealth_idx+urbanity+AGE_IN_YEARS+log(crp)+log(agp)+BMI_cat,design=DHSdesign, family=gaussian())

anova(m0,m1)

coef(summary(m1))
