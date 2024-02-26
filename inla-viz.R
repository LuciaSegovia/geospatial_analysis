
library(ggplot2)
library(ggregplot)
#install.packages("MCMCglmm")
library(stringr)
library(magrittr)

#ggplot(plasma_se, aes(Longitude, Latitude )) + 
#  geom_jitter(aes(colour = factor(survey_cluster1))) + coord_fixed() + 
#  labs(colour = "survey_cluster1")


INLADICFig(models)

Efxplot(models)

summary(models[[3]])
