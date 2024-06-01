### 
#   
#   
#   This script contains all the visuals used in the manuscript:
#     The optimal way of aggregating geo-referenced Se concentration
#     for nutritional assessment purposes
#   
#   
###########


library(ggplot2)

# Data

file <- grep("plasma.*v2.0.0", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

dist <- readRDS(here::here("data", "inter-output", "cluster-distance-to-mwi-lakes.RDS"))

plasma_se <- readRDS(here::here("data", "inter-output", "model",
                                file[9])) %>%
  # Joining the variable distance to inland water body
  left_join(., dist) 

# Renaming variable and checking indv. data
plasma_se <- dplyr::rename(plasma_se, Plasma_Se = "selenium")
plasma_se$survey_cluster1 <- as.character(plasma_se$survey_cluster1)

# Variables and labels ----
# Discrete variables (factors) from number to text
lab_region <- c(`1` = "Northern", `2` = "Central", `3` = "Southern")
lab_reside <- c(`1` = "Urban", `2` = "Rural")
lab_malaria <- c(`1` = "Positive", `2` = "Negative")

# Variables and units (axis - text)
plasma_lab <- expression(paste("Plasma Se conc. (ng  ",  mL^{-1}, ")"))
plasma_units <- expression(paste("ng  ",  mL^{-1}))
bmi_lab <- expression(paste("BMI (kg /  ",  m^{2}, ")"))

# Colour choices ----

# 1) Plasma
"firebrick4"

# 2) Urban and rural
col_break <- c("2" = "#00BFC4", "1" = "#F8766D")


## Plasma: Point graph -------

plasma_se %>% ggplot(aes(survey_cluster1, Plasma_Se, colour = urbanity)) +
  geom_point() +  
  scale_colour_manual("", values = col_break, labels = lab_reside)+
  coord_flip() +
  facet_wrap(~region, labeller = as_labeller(lab_region), 
             scales = "free_y") +
  theme_minimal()+
  labs(y = plasma_units, x ="") +
  theme(legend.position = "top",
        #legend.justification = "right" 
        strip.background = element_rect(color = "lightgray", linewidth = 0.2), 
        panel.border = element_rect(fill = "transparent", # Needed to add the border
                                    color = "lightgray", linewidth = 0.2)
        )





