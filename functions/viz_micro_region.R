

### This visualisation fuction takes one micronutrient, and two sorting 
## variables faceted by three regions in Malawi

# df = dataset
#micro = micronutient (numeric)
# var_sorting = variable for subsetting (character/interger)
# var_colouring = variable for colouring (character/interger)
# unit_micro = label for the micronutrient (text or expresion) 
# label_sorting = label for sorting variable
# col_break = colours for the variable colouirng 
# col_labels = meaning of each colour

## Examples:
# Custom legend colour & labels
col_break <- c("1" = "#00BFC4", "2" = "#F8766D")
col_labels <- c("1" = "Urban", "2" = "Rural")
unit_micro <- expression(paste("Plasma Se conc. (ng  ",  mL^{-1}, ")"))



micro_region <- function(df, micro, var_sorting, var_colouring, unit_micro, label_sorting, col_break, col_labels){

plot <- ggplot(data = df, 
       mapping = aes(x = {{micro}}, y ={{var_sorting}},
                     colour = {{var_colouring}})) +
  geom_boxplot() +
  theme_bw() +
  coord_flip() +
  scale_colour_manual("", 
                      values =  {{col_break}},
                      # breaks = col_break,
                      labels = {{col_labels}}) +
  facet_wrap(~region, labeller = as_labeller(c(`1` = "Northern", 
                                               `2` = "Central", 
                                               `3` = "Southern")),
             scales = "free_x") +
  # scale_x_discrete(label = labels) +
  labs(
    x= unit_micro,
    y =label_sorting ) + 
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 10, angle =30), 
        legend.position = "top") 

return(plot)

}
