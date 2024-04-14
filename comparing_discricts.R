


library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # visualisation


maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               "pred-maize-district.RDS"))


Krop <- read.csv("Se_raw_block_kriging_v.1.0.0.csv")


maize.df %>% left_join(., Krop, by = c("ADM2_EN" = "District")) %>% 
                         select(ADM2_EN, Se_mean, exp_block) %>% 
  pivot_longer(
    cols = c("Se_mean", "exp_block"), 
    names_to = "method", 
    values_to = "Se_conc"
  ) %>% 
ggplot(aes(ADM2_EN, Se_conc, colour = method)) +
  geom_point() + coord_flip()


