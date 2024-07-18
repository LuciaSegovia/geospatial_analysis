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
library(dplyr)
library(tidyr)
library(hrbrthemes)
#library(summarytools) # didn't work


# Data -------

## Maize aggregations -----
# Maize Se conc. (from 01_maize-aggregation.R)

(file <- grep("pred-maize.*._v2", list.files(here::here("data", "inter-output", "aggregation")), 
              value = TRUE))

temp <- paste(here::here("data", "inter-output", "aggregation"), file, sep = "/")

myfiles = lapply(temp, readRDS)

# maize.df <- do.call(rbind, myfiles)
  
# Adding a column with the aggregation
#myfiles[[1]][, "aggregation"] <- file[1]

for(i in 1:length(file)){myfiles[[i]][, "aggregation"] <- file[i]}

# Load the maize Se conc. aggregated dataset
maize.df <- readRDS(here::here("data", "inter-output", "aggregation", 
                               file[i]))

## Modelled plasma + maize ----

file <- grep("plasma.*v2.0.0", list.files(here::here("data", "inter-output", "model")), 
             value = TRUE)

temp <- paste(here::here("data", "inter-output", "model"), file, sep = "/")

myfiles = lapply(temp, readRDS)

# Adding a column with the aggregation
for(i in 1:length(file)){myfiles[[i]][, "aggregation"] <- file[i]}

# Converting list into dataframe
data.df <- do.call(rbind, myfiles)

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
# Not to be shown bc issues of annonymity
plasma_se %>% 
  ggplot(aes(survey_cluster1, Plasma_Se, colour = urbanity)) +
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

## Plasma: Point and error bar graph -------

plasma_se %>%  group_by(survey_cluster1, urbanity) %>% 
  summarise(Se_mean = mean(Plasma_Se, na.rm = TRUE),
            Se_sd = sd(Plasma_Se, na.rm = TRUE)) %>% 
ggplot(aes( as.factor(survey_cluster1), Se_mean,
            colour = urbanity)) +
  geom_point( position=position_dodge(0.75),)  + 
  geom_errorbar(aes(ymin=Se_mean-Se_sd, ymax=Se_mean+Se_sd), width=.2,
                position=position_dodge(0.75)) +
  scale_colour_manual("", values = col_break, labels = lab_reside) +

## Figure 2: Maize and Plasma: Point and error bar graph -------
# Adapted from: https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html

# Value to get the two values in the same scale.
coeff <- 1000
  
  plasma_se %>%  group_by(survey_cluster1, region) %>% 
  summarise(PlasmaSe_mean = mean(Plasma_Se, na.rm = TRUE),
            PlasmaSe_sd = sd(Plasma_Se, na.rm = TRUE), 
            MaizeSe_mean = mean(Se_mean, na.rm = TRUE),
          MaizeSe_sd = mean(Se_sd, na.rm = TRUE)) %>%                  
  ggplot(aes(x = as.factor(survey_cluster1))) +
  
  # Maize Se points and bars 
  geom_point(aes(y = MaizeSe_mean), position=position_dodge(0.75),  colour = "#DF0D5B")  + 
  geom_errorbar(aes(ymin=MaizeSe_mean-MaizeSe_sd, 
                    ymax=MaizeSe_mean+MaizeSe_sd), width=.2,
                position=position_dodge(0.75), colour = "#DF0D5B") +
  
  # Plasma Se points and bars
  geom_point(aes(y = PlasmaSe_mean/coeff), position=position_dodge(0.75))  + 
  geom_errorbar(aes(ymin=(PlasmaSe_mean-PlasmaSe_sd)/coeff, 
                    ymax=(PlasmaSe_mean+PlasmaSe_sd)/coeff), width=.2,
                position=position_dodge(0.75)) +
  
    facet_wrap(~region, labeller = as_labeller(lab_region), scales = "free_x") +
  labs(x = "cluster ID")+
      # Custom the Y scales:
  scale_y_continuous(
    
    # Features of the first axis
    name = "Maize Se conc. (mcg/Kg)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Plasma Se conc.(ng/L)")
  ) +
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 8, angle = 60, 
                                 hjust = 0.5, vjust = 0.5),
       axis.title.y = element_text(colour = "#DF0D5B", size =13),
      axis.title.y.right = element_text(colour = "black", size=13)
    ) +
    ggtitle("Maize Se conc. for each EA group, and plasma Se conc.")
  
  
  ## Figure 3: Maize and Plasma: correlation -------
  plasma_se %>%  
    group_by(survey_cluster1, region, dist_name ) %>% 
    summarise(PlasmaSe_mean = mean(Plasma_Se, na.rm = TRUE),
              PlasmaSe_sd = sd(Plasma_Se, na.rm = TRUE), 
              MaizeSe_mean = mean(Se_mean, na.rm = TRUE),
              MaizeSe_sd = mean(Se_sd, na.rm = TRUE)) %>% 
    ggplot(aes( MaizeSe_mean, PlasmaSe_mean, colour = dist_name )) +
    geom_point() +  
    facet_wrap(~region, labeller = as_labeller(lab_region), scales = "free_x") 
  
  plasma_se %>%  
    ggplot(aes( Se_mean, Plasma_Se, colour = dist_name )) +
    geom_point() +  
    facet_wrap(~region, nrow = 3, labeller = as_labeller(lab_region), 
               scales = "free_x") + theme_minimal()
  
  # checking
  plasma_se %>%  group_by(survey_cluster1, region, dist_name ) %>% 
    summarise(PlasmaSe_mean = mean(Plasma_Se, na.rm = TRUE),
              PlasmaSe_sd = sd(Plasma_Se, na.rm = TRUE), 
              MaizeSe_mean = mean(Se_mean, na.rm = TRUE),
              MaizeSe_sd = mean(Se_sd, na.rm = TRUE)) %>% 
   arrange(desc(PlasmaSe_sd)) %>% filter(region == 1) %>% View()
  

# Table 1 -----
## To-Do:
### Adding survey weight
  ## Adding areas of the aggregations
  
data.df %>% 
  dplyr::select(survey_cluster1, selenium, AGE_IN_YEARS, crp, agp, urbanity,  
                 wealth_idx, Se_mean, 
                          aggregation) %>% 
  rename(Age = "AGE_IN_YEARS", PlasmaSe = "selenium") %>% 
  mutate(aggregation = gsub("plasma-pred-maize-", "", aggregation), 
         aggregation = gsub("_v2.0.0.RDS", "", aggregation),
         survey_cluster1 = as.character(survey_cluster1)) %>% 
  pivot_wider(., 
               names_from = aggregation,
               values_from = Se_mean) %>% 
  filter(!is.na(PlasmaSe), !is.na(wealth_idx), !is.na(crp)) %>% 
  summarise(across(where(is.numeric),
                   list( mean= mean, SD = sd,
                        median = median, min = min, max = max))) %>% 
  pivot_longer( everything(),
cols_vary = "slowest",
names_to = c("variable", ".value"),
names_pattern = "([[:alnum:]]+).([[:alnum:]]+)") %>% 
  write.csv(., here::here("output", "dummy-summary-table.csv"), row.names = FALSE)

## SM: Maize aggreg by cluster ----------------

"#DF0D5B"
"#0CCE0C"

col_scale <- c(gray(0:7 / 8),"#B00BA2", "#DF0D5B")

plot <- list()

for(i in 1:3){

plot[[i]] <- data.df %>% 
  dplyr::select(survey_cluster1, selenium, AGE_IN_YEARS, crp, agp, urbanity,  
                wealth_idx, Se_mean, Se_sd, region, dist_name,
                aggregation) %>% 
  rename(Age = "AGE_IN_YEARS", PlasmaSe = "selenium") %>% 
  mutate(aggregation = gsub("plasma-pred-maize-", "", aggregation), 
         aggregation = gsub("_v2.0.0.RDS", "", aggregation),
         survey_cluster1 = as.character(survey_cluster1)) %>% 
  filter(!is.na(PlasmaSe), !is.na(wealth_idx), !is.na(crp)) %>% 
  filter(region == i) %>% 
  ggplot(aes( as.factor(survey_cluster1), Se_mean, colour = aggregation)) +
  geom_point( position=position_dodge(0.75))  + 
  geom_errorbar(aes(ymin=Se_mean-Se_sd, ymax=Se_mean+Se_sd), width=.2,
                position=position_dodge(0.75)) +
   #scale_colour_grey() +
  scale_colour_manual(values = col_scale)+
  #scale_colour_manual(values = c("cluster" = "#B00BA2", "district" = "#DF0D5B"))+
  theme_light() +   coord_flip() + 
  #  facet_grid(rows = vars(region),
  facet_wrap(~dist_name, 
           #  labeller = as_labeller(lab_region),
             scales = "free_y") + 
  labs(title = paste(lab_region[i], "region"),
    x = "",
       y = expression(paste("Maize Se conc. (mg  ",  Kg^{-1}, ")"))) + 
  #  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10)) 
}
