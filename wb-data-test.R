
# Cleaning the environment
rm(list = ls())

# Loading libraries
library(sf) # spatial data manipulation
library(spdep) # grid and neighbours
library(dplyr) # data wrangling
library(car) # lm VIF (multicolinearity test)
source(here::here("functions", "CEPHaStat_3.R")) #stat functions

## Loading the data

file <- grep("plasma", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

i =9

plasma_se <- readRDS(here::here("data", "inter-output", "model", file[i]))

#dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-wb.RDS"))
dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-mwi-lakes.RDS"))

data.df <- plasma_se %>% left_join(., dist) 

 data.df$dist_to_wb2 <- data.df$dist_to_wb

data.df$dist_to_wb2[data.df$dist_to_wb == 0] <-  200
# conventional skewness range of [-1, 1] 
# octile skewness range of [−0.2, 0.2]
summaplot(data.df$dist_to_wb)
summa(data.df$dist_to_wb)
summaplot(log(data.df$dist_to_wb2))
summa(log(data.df$dist_to_wb2))

plot(log(data.df$dist_to_wb2), data.df$selenium)
plot(log(data.df$dist_to_wb2), log(data.df$Se_mean))

cor(log(data.df$dist_to_wb2), data.df$Se_mean)
cor(log(data.df$dist_to_wb2), data.df$selenium)
cor(data.df$Se_mean, data.df$selenium)
cor(data.df$AGE_IN_YEARS, data.df$selenium)

# Testing multicolinearity

#define multiple linear regression model
model <- lm(log(selenium) ~ log(Se_mean) +
              wealth_quintile + BMI + urbanity + 
              AGE_IN_YEARS +
              log(crp) + log(agp) + log(dist_to_wb2), data=data.df)

#calculate the VIF for each predictor variable in the model
vif(model)


# Testing for colinearity ----
# Correlation matrix ----
#install.packages("corrplot")

corrplot::corrplot(cor(na.omit(data.df[, c("selenium", "Se_mean", 
                          "BMI", "AGE_IN_YEARS",
                                   "crp", "agp", "dist_to_lake")])),
                                 method = "number")


# Testing multicolinearity

#define multiple linear regression model
model2 <- lm(log(selenium) ~ log(Se_mean) +
              wealth_quintile + BMI + urbanity + Malaria_test_result +
              AGE_IN_YEARS + log(crp) + log(agp) + 
              log(dist_to_lake), data=data.df)

#calculate the VIF for each predictor variable in the model
vif(model2)
olsrr::ols_vif_tol(model2)
olsrr::ols_eigen_cindex(model2)
olsrr::ols_plot_resid_fit_spread(model2)
olsrr::ols_plot_obs_fit(model2)
olsrr::ols_plot_diagnostics(model2)


# 
olsrr::ols_vif_tol(models[[9]])
