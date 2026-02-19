## Using penguin data
## add a new col (fatstat)
## for penguins weight more than 5000g --> 
## for penguins weight <= 5000g and more than 3000g -->
## for penguins weight <= 3000g -->

install.packages('palmerpenguins')
library(dplyr)
library(palmerpenguins)
library(ggplot2)
library(purrr)
df <- pedf <- pedf <- penguins

df <- df %>%
  mutate(fatstat = case_when(
    body_mass_g > 5000 ~ "Fat",
    body_mass_g < 3000 ~ "Skinny",
    body_mass_g <= 5000 & body_mass_g > 3000 ~ "Normal"
    ))

df[df$body_mass_g <= 5000 & df$body_mass_g > 3000, 'fatstat']



# Plotting functions
plot(df$bill_length_mm, df$body_mass_g)

ggplot(
  aes(x=bill_length_mm, y=body_mass_g),
  data=df) +
  geom_point()


df %>% 
  ggplot(
    aes(x=bill_length_mm, 
        y=body_mass_g,
        color=sex,
        shape=species,
        size=bill_depth_mm)) +
  geom_point() +
  theme_minimal()



## Take a look of your new penguin data
## make a cool graph
install.packages('ggridges')
library(ggridges)
## Ridge plot
na.omit(df) %>% 
  ggplot(aes(x=body_mass_g,
             y=fatstat,
             fill=fatstat)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = 'none')

## Ridge Overlapping density plot
na.omit(df) %>% 
  ggplot(aes(x=body_mass_g,
             color=species,
             fill=species)) +
  geom_density(alpha = 0.5) 



na.omit(df) %>% 
  ggplot(aes(x=body_mass_g,
             y=species,
             fill=species)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges() + 
  theme(legend.position = 'none')


na.omit(df) %>% 
  ggplot(aes(y=body_mass_g,
             x=species, fill=species)) +
  geom_violin()

## Find the total mass of all the penguins matching 'Gentoo'
gentoo <- df[df$species == 'Gentoo' & !is.na(df$body_mass_g), ]
sum(gentoo$body_mass_g)

df %>% 
  filter(species=='Gentoo') %>% 
  pluck('body_mass_g') %>% 
  sum(na.rm=T)




## Plot average body mass of penguins by sex and species
na.omit(df) %>% 
  ggplot(aes(y=body_mass_g, 
             x=species, 
             fill=sex)) +
  geom_bar(stat = 'identity',position='dodge')

grouped_means <- na.omit(df) %>%
  group_by(species, sex) %>%
  summarise(mean_mass = mean(body_mass_g, na.rm = TRUE))

grouped_means %>% 
  ggplot(aes(y=mean_mass, 
             x=species, 
             fill=sex)) + 
  geom_bar(stat='identity', position='dodge')





