library(tidyverse)
library(gganimate)
library(ggpubr)
library(jpeg)
df <- read.csv('../../Data/Soil_Predators.csv')
names(df)
View(df)

pred_cols <-  grep("Consumption", names(df), value = 1)
pred_cols

df <- df %>% 
  pivot_longer(cols = all_of(pred_cols),
               names_to = "Consumed_species",
               values_to = "Consumed_amount")


img.file <- 'download.jpeg'
img <- jpeg::readJPEG(img.file)

my_colors <- c("#00cf00", "#00dd00", "#00ee00", "#00cc00",
               "#00bb00", "#00aa00","#00af00", "#00da00", 
               "#00cb00", "#00ca00")


na.omit(df) %>% 
  ggplot(aes(x = X_measured., fill = Predator_species)) +
  background_image(img) +
  geom_density(linewidth = 0) +
  scale_fill_manual(values = my_colors) +
  theme_minimal()

ggsave('final_ugly_plot.png')


