# Exam 2 #
library(tidyverse)
library(skimr)
library(janitor)
library(easystats)
# Question 1: Read in the Data ####
df <- read.csv('unicef-u5mr.csv')
str(df)
skim(df)

# Question 2: Turn into tidy format ####
df <- df %>% 
  pivot_longer(cols = starts_with('U5MR.'),
               names_to = 'Year',
               names_prefix = 'U5MR.',
               values_to = 'U5MR') %>% 
  mutate(Year = as.integer(Year))
str(df)

# Question 3: Plot each countries mortality over time ####
q3_plot <- df %>% 
  ggplot(aes(x = Year, y = U5MR)) +
  facet_wrap(~Continent) +
  geom_line(aes(group = CountryName))

# Question 4: Save the plot ####
ggsave('JOHNSON_Plot_1.png', q3_plot)

# Question 5: Create plot of mean by continent over each year

df_summary <- df %>% 
  group_by(Continent, Year) %>% 
  summarize(
    Mean_U5MR = mean(U5MR, na.rm = TRUE),
    .groups = 'drop'
  )

q5_plot <- df_summary %>% 
  ggplot(aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 2) +
  theme_bw()

# Question 6: Save that plot ####
ggsave('JOHNSON_Plot_2.png', q5_plot)

# Question 7: Create 3 models
mod1 <- glm(U5MR ~ Year, data = df)
mod2 <- glm(U5MR ~ Year + Continent, data = df)
mod3 <- glm(U5MR ~ Year*Continent, data = df)

# Question 8: Compare model performance ####
summary(mod1)
summary(mod2)
summary(mod3)
compare_performance(mod1, mod2, mod3)
# Model 2 is probably the best model here, even though the R2 isn't the best, 
# it has a much better AIC, AICc, BIC than the other model that has a slightly 
# better R2 score

# Question 9: Plotting the 3 models predictions ####

df$mod1 <- predict(mod1, df, type='response')
df$mod2 <- predict(mod2, df, type='response')
df$mod3 <- predict(mod3, df, type='response')
df <- df %>% pivot_longer(
  cols = c(mod1, mod2, mod3),
  names_to = 'model',
  values_to = 'prediction'
)
df %>% 
  ggplot(aes(x = Year, y = prediction, color = Continent)) +
  facet_wrap(~model) +
  geom_line() +
  theme_bw() +
  labs(y = 'Prediction')

# Question 10: Make a prediction on Ecuador in 2020 ####
pred_val <- data.frame(
  Continent = 'Americas',
  Year = 2020,
  CountryName = 'Ecuador'
)
bonus_pred <- predict(mod2, pred_val)
actual <- 13

