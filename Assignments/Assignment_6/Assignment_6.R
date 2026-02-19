library(tidyverse)
library(gganimate)
df <- read.csv("../../Data/BioLog_Plate_Data.csv") 


#### Problem 1 ####
df <- df %>% 
  pivot_longer(
    cols = c(Hr_24, Hr_48, Hr_144),
    names_to="Time",
    values_to="Absorbance"
  ) %>% 
  mutate(Time = case_when(
    Time == 'Hr_24' ~ 24,
    Time == 'Hr_48' ~ 48,
    Time == 'Hr_144' ~ 144
  ))

#### Problem 2 ####
df <- df %>% 
  mutate(Source.Type = case_when(
    Sample.ID == 'Clear_Creek' | Sample.ID == 'Waste_Water' ~ 'Water',
    Sample.ID == 'Soil_1' | Sample.ID == 'Soil_2' ~ 'Soil'
  ))


#### Problem 3 ####
df_mean <- df %>%
  filter(Dilution == 0.1) %>%
  group_by(Substrate, Time, Source.Type) %>%
  summarise(
    Absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  )

df_mean %>% 
  ggplot(aes(
    x = Time,
    y = Absorbance,
    color = Source.Type)) +
  facet_wrap(~ Substrate) +
  geom_smooth(na.rm=T, formula = y ~ x) +
  labs(subtitle='Just dilution 0.1',
       color = 'Type') +
  theme_minimal() +
  theme(strip.text = element_text(size=5),
        axis.text.y = element_text(size=7))
    



#### Problem 4 ####
df %>% 
  filter(Substrate == 'Itaconic Acid') %>% 
  group_by(Sample.ID, Dilution, Time, Source.Type) %>%
  summarise(Mean_absorbance = mean(Absorbance, na.rm = TRUE),
            .groups = "drop"
  ) %>%
  ggplot(aes(x = Time, 
             y = Mean_absorbance,
             color = Sample.ID,
             group = Sample.ID)) +
  geom_line() +
  facet_wrap(~Dilution, ncol = 3) +
  labs(color = 'Sample ID') +
  theme_minimal(base_size=18)


p4 <- df %>% 
  filter(Substrate == 'Itaconic Acid') %>% 
  group_by(Sample.ID, Dilution, Time, Source.Type) %>%
  summarise(Mean_absorbance = mean(Absorbance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Time, 
             y = Mean_absorbance,
             color = Sample.ID,
             group = Sample.ID)) +
  geom_line() +
  facet_wrap(~Dilution, ncol = 3) +
  labs(color = 'Sample ID') +
  theme_minimal(base_size = 18) +
  transition_reveal(Time)


animate(p4,
        fps = 10,
        duration = 10, 
        width = 800, 
        height=900)
anim_save("itaconic_acid.gif")






