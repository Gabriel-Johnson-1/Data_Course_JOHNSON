## download height.xlsx
## make it tidy, make a plot and explain it
library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
path = 'Data/height.xlsx'
df <- read_xlsx(path)
str(df)
df <- df %>% 
  pivot_longer(cols = c(male, female),
               names_to = 'sex',
               values_to = 'height') %>% 
  separate_wider_delim(col = 'height', names = c('feet', 'inch'), delim = "'") %>% 
  mutate(feet = as.integer(feet),
         inch = as.integer(inch)) %>% 
  mutate(height = feet + 12*inch)

df %>% ggplot(aes(x = sex, y = height)) +
  geom_violin() +
  geom_boxplot(width = 0.2)

df %>% ggplot(aes(x = height, color = sex, fill = sex)) +
  geom_density(alpha = 0.4)




