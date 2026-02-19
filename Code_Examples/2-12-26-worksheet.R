library(palmerpenguins)
library(tidyverse)

## plot average body mass of penguins by sex and species
## try to add error bar


df <- penguins
p1 <- df %>% 
  filter(!is.na(sex)) %>% 
  group_by(species, sex) %>% 
  summarise(avg_mass = mean(body_mass_g),
            sd_mass = sd(body_mass_g)) %>% 
  ggplot(aes(x=species, 
             y=avg_mass, 
             fill=sex)) +
  geom_bar(stat = 'identity', 
           position='dodge') +
  geom_errorbar(aes(ymin=avg_mass - sd_mass, 
                    ymax=avg_mass + sd_mass),
                position = position_dodge2(width=0.5, padding = 0.5))



p2 <- df %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = as.factor(year),
             y= body_mass_g,
             color=species)) +
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = .75),
              alpha=0.5)
  

## in penguin dataset add a new col called mass_100 and save to a new obj
## value = body_mass_g + 100

new_df <- df %>% 
  mutate(mass_100 = body_mass_g + 100)

new_df %>% 
  select(-c(species, mass_100))


## Recreate the graph on the screen

p3 <- df %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(x=bill_depth_mm, 
             y=body_mass_g, 
             color=sex)) +
  geom_point(alpha=0.75, size = 4) +
  labs(x = 'Bill depth (mm)',
       y = 'Body mass (g)',
       color = "Sex") +
  facet_wrap(~species, ncol=3) +
  scale_color_viridis_d(end=0.8) +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text = element_text(face = 'bold'),
        strip.text = element_text(face = 'bold', size = 14))


## Read Data/DatasaurusDozen.tsv
## exam and make a good graph


df <- read_tsv('Data/DatasaurusDozen.tsv')

df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~dataset)

df %>% 
  group_by(dataset) %>% 
  summarise(avg_x = mean(x),
            sd_x = sd(x),
            avg_y = mean(y),
            sd_y = sd(y)
            )
df %>% 
  filter(dataset=='dino') %>% 
  ggplot(aes(x = x, y = y, color=x))+
  geom_point(size=6) +
  labs(title = 'Duh\'-ryl d\'Dayeneauxgh',
       subtitle = '"My arm are too small"') +
  scale_color_viridis_c() +
  theme_void() +
  theme(legend.position = 'none')
  

unique(df$dataset)



