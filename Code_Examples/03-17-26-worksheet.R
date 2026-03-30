library(tidyverse)
library(janitor)
library(readxl)
#### Blood Pressure Data ####
path = 'Data/messy_bp.xlsx'

df <- read_xlsx(path, skip=3)
View(df)

df %>% 
  select(-c('HR...9', 'HR...11', 'HR...13'))

bp <- df %>% 
  select(-starts_with('HR')) %>% 
  pivot_longer(cols = starts_with('BP'),
               names_to = 'visit',
               values_to = 'BP') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3)) %>%
  separate(BP, into = c('sys', 'dia'))



## clean HR data and put them back
## check the data and see if there's anything needing to be fixed
hr <- df %>% 
  select(-starts_with('BP')) %>% 
  pivot_longer(cols = starts_with('HR'),
               names_to = 'visit',
               values_to = 'HR') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3))

df_clean <- full_join(bp, hr) %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, 
                     month_of_birth, 
                     day_birth, 
                     sep = '-') %>% as.Date())

df_clean %>% 
  ggplot(aes(x = visit, y = hr)) +
  geom_line() 

df_clean_2 <- df_clean %>% 
  clean_names() %>% 
  mutate(DOB = paste(year_birth, month_of_birth, day_birth, 
                     sep = '-') %>% as.Date()) %>% 
  mutate(race = case_when(race == 'Caucasion' | race == 'White' ~ 'Caucasian',
                          TRUE ~ race))

#### Bird Measurement Data ####
library(skimr)
path = './Data/Bird_Measurements.csv'
df <- read.csv(path)
View(df)



