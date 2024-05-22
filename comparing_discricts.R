


library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # visualisation

cluster.df <- readRDS(here::here("data", "inter-output", 
                                 "aggregation", "master-cluster-admin-level.RDS"))

names(cluster.df)

maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               "pred-maize-district.RDS"))


maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               "pred-maize-cluster_v2.0.0.RDS")) %>% 
  left_join(., readRDS(here::here("data", "inter-output", "aggregation", 
                                  "pred-maize-buffer10_v2.0.0.RDS")), 
            by = "survey_cluster1")

names(maize.df)

maize.df <- maize.df %>% left_join(., cluster.df %>%
                         select(survey_cluster1, ADM1_EN, ADM2_EN)) %>% 
  distinct()

maize.df$survey_cluster1 <- as.character(maize.df$survey_cluster1)

#Krop <- read.csv("Se_raw_block_kriging_v.1.0.0.csv")

#vari.col <- c("Se_mean", "exp_block")
vari.col <- c("Se_mean.x", "Se_mean.y")

maize.df %>% 
#  left_join(., Krop, by = c("ADM2_EN" = "District")) %>% 
#                         select(ADM2_EN, Se_mean, exp_block) %>% 
  pivot_longer(
    cols = vari.col, 
    names_to = "aggregation", 
    values_to = "Se_conc"
  ) %>% 
ggplot(aes(survey_cluster1, Se_conc, colour = aggregation)) +
  geom_point() + coord_flip() +
  facet_wrap(~ADM1_EN, 
             scales = "free_y") # +
#  facet_grid(rows = vars(ADM2_EN),
#             cols = vars(ADM1_EN),
#              axis = "all_y",
#             scales = "free_y", drop = TRUE)
#


