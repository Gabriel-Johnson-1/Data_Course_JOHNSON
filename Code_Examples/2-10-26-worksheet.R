## make a cool plot using penguin data (make sure no NA)
## Manually set colors as ugly/annoying as possible
## Add title, fix axis labels
## save to local directory
library(palmerpenguins)
library(tidyverse)
df <- penguins
head(df)
df <- drop_na(df)
head(df)

my_colors = c("#ff00ff", "#00ff00")

p <- df %>% 
  ggplot(aes(y=body_mass_g, x = sex, fill=sex, color=sex)) +
  geom_boxplot(color='black', 
               fill='white', 
               alpha=0.5, 
               outlier.size = 0, 
               outlier.alpha = 0) +
  geom_jitter(alpha=0.5) +
  facet_wrap(~species) +
  labs(title = 'Body Mass in Grams by Sex',
       subtitle = 'for each species',
       y = 'Body Mass') +
  theme_dark() +
  theme(plot.title=element_text(hjust=0.5, face='bold.italic'),
        plot.subtitle = element_text(hjust=0.37, face='oblique')) +
  scale_color_manual(values = my_colors)
p

ggsave('2-10-26-bad-colors.png', p)


# Make a plot to show penguin weight change across 3 years
df %>% 
  ggplot(aes(x=as.factor(year), y=body_mass_g, color=as.factor(year)))+
  geom_violin(aes(fill=as.factor(year)), alpha=0.2) +
  geom_boxplot(width=0.1, color='black') +
  geom_jitter(width=0.1, color='black', alpha=0.3)+
  theme_minimal() +
  labs(title = 'Body Mass in Grams',
       subtitle = 'From 2007-2009',
       x = 'Year',
       y= 'Body Mass G',
       color="Year",
       fill='Year')

names(df)



# Changing to a different dataset
data_dir <- "/home/gabe-j/Desktop/school/current_classes/data_analysis_for_bio/Data_Course_JOHNSON/Data/"
df <- read.csv(paste0(data_dir, 'wide_income_rent.csv'))

summary(df)
head(df)
View(df)



















