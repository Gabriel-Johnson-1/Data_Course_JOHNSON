## This is a script for bird data analysis

# Gabriel Johnson
# 03-19-2026

# load required packages ####
library(tidyverse)
library(janitor)
library(skimr)

# load data
df <- read.csv('./Data/Bird_Measurements.csv')
df <- read_csv('./Data/Bird_Measurements.csv')
View(df)

# skimr functionality
skim(df)

## data cleaning
keepers <- c('Family', 'Species_number', 'Species_name', 'English_name', 
             'Clutch_size', 'Egg_mass')

df_filtered <- df %>% 
  select(all_of(keepers), starts_with('M_'), -ends_with('_N')) %>% 
  clean_names()

View(df_filtered)




