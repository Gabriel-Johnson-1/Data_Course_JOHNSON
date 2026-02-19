library(tidyverse)
df <- read.delim('../Data/DatasaurusDozen.tsv')
head(df)
unique(df$dataset)

df %>% 
  ggplot(aes(x = x,
             y = y,
             color = 'dataset')) +
  facet_wrap(~dataset) +
  geom_point() +
  

df %>% 
  group_by(dataset) %>% 
  summarize(mean_x = mean(x), 
            sd_x = sd(x),
            max_x = max(x),
            mean_y = mean(y),
            sd_y = sd(y),
            max_y = max(y))


library(GGally)
ggpairs(df)



library(gapminder)
library(gganimate)
df <- gapminder
View(df)
head(df)
names(df)
length(unique(df$country))

df %>% 
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(aes(size = pop),
              alpha = 0.3,
              color = 'blue',
              width = 0.2) +
  labs(title = 'Year: {closest_state}') +
  theme(legend.position = 'none')

p <- df %>% 
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.alpha = 0, width = 0.5) +
  geom_jitter(aes(size = pop),
              alpha = 0.5,
              color = 'blue',
              width = 0.2) +
  labs(title = 'Life Expactancy in {closest_state}') +
  theme(legend.position = 'none') +
  transition_states(year)



animate(p, width=800, height = 800)

anim_save('life-expectancy-over-years.gif')








