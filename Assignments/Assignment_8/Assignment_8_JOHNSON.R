library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

#### 1. Load the data set ####
df <- read.csv('../../Data/mushroom_growth.csv')
str(df)
unique(df$Species)
unique(df$Light)
unique(df$Nitrogen)
unique(df$Humidity)
unique(df$Temperature)

#### 2. Create plots ####
# Species Plots
df %>% 
  ggplot(aes(x = GrowthRate, 
             color = Species, 
             fill=Species)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Growth Rate by Species")


df %>% 
  ggplot(aes(x = GrowthRate, 
             y = Species)) +
  geom_boxplot() +
  labs(title = "Boxplots of Growth Rate by Species")

# Light plots
df %>% 
  ggplot(aes(x = GrowthRate, 
             color = as.factor(Light), 
             fill = as.factor(Light))) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Growth Rate by Light")


df %>% 
  ggplot(aes(x = GrowthRate, 
             y = as.factor(Light))) +
  geom_boxplot() +
  labs(title = "Boxplots of Growth Rate by Light")

df %>% 
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_point() +
  labs(title = "Scatter plot of GrowthRate by Light")

# Nitrogen plots
df %>% 
  ggplot(aes(x = GrowthRate, 
             color = as.factor(Nitrogen), 
             fill = as.factor(Nitrogen))) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Growth Rate by Nitrogen")

df %>% 
  ggplot(aes(y = GrowthRate, 
             x = as.factor(Nitrogen))) +
  geom_boxplot() +
  labs(title = "Boxplots of Growth Rate by Nitrogen")

df %>% 
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  labs(title = "Scatter plot of GrowthRate by Nitrogen")

# Humidity plots
df %>% 
  ggplot(aes(x = GrowthRate, 
             color = Humidity, 
             fill = Humidity)) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Growth Rate by Humidity")

df %>% 
  ggplot(aes(x = GrowthRate, 
             y = Humidity)) +
  geom_boxplot() +
  labs(title = "Boxplots of Growth Rate by Humidity")

# Temperature plots
df %>% 
  ggplot(aes(x = GrowthRate, 
             color = as.factor(Temperature), 
             fill = as.factor(Temperature))) +
  geom_density(alpha = 0.2) +
  labs(title = "Density Plot of Growth Rate by Temperature")

df %>% 
  ggplot(aes(y = GrowthRate, 
             x = as.factor(Temperature))) +
  geom_boxplot() +
  labs(title = "Boxplots of Growth Rate by Temperature")

df %>% 
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  labs(title = "Scatter plot of GrowthRate by Temperature")


#### 3. Define 4 different models ####

mod1 <- lm(GrowthRate ~ Light, data = df)
summary(mod1)

mod2 <- lm(GrowthRate ~ Nitrogen, data = df)
summary(mod2)

mod3 <- lm(GrowthRate ~ Temperature, data = df)
summary(mod3)

mod4 <- lm(GrowthRate ~ Species, data = df)
summary(mod4)

mod5 <- lm(GrowthRate~Species+Light+Nitrogen+Temperature+Species, data = df)
summary(mod5)

mods <- list(mod1, mod2, mod3, mod4, mod5)

#### 4. Calculating the Mean Squared Error or each Model ####
perfs <- map(mods, performance) %>% reduce(full_join)
mse <- perfs$RMSE**2
paste('mse:', mse)

#### 5. Select the best model ####
best_mod <- mod5

#### 6. Adds predictions based on 


