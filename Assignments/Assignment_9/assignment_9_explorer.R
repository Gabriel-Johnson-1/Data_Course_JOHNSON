library(tidyverse)
library(skimr)
library(modelr)
library(easystats)

# Loading in the data ####
df <- read.csv('../../Data/GradSchool_Admissions.csv')
skim(df)
summary(df)

# Plotting univariate distributions ####

# Admittance status
df %>%
  ggplot(aes(x = as.factor(admit))) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = after_stat(count)), 
            vjust = 1.5,
            color = 'white') +
  labs(title = "Counts of each admittance status")

# Gre
df %>% 
  ggplot(aes(x = gre)) +
  geom_density(fill = 'grey') +
  labs(title = "Distribution of GRE Scores")

df %>% 
  ggplot(aes(x = gre)) +
  geom_boxplot() +
  labs(title = "Boxplot of GRE Scores")

# GPA
df %>% 
  ggplot(aes(x = gpa)) + 
  geom_density(fill = 'grey') +
  labs(title = "Distribution of GPA")

df %>% 
  ggplot(aes(x = gpa)) +
  geom_boxplot() +
  labs(title = "Boxplot of GPA Scores")

# Rank
df %>% 
  ggplot(aes(x = rank)) + 
  geom_bar() +
  geom_text(stat = "count",
            aes(label = after_stat(count)), 
            vjust = 1.5,
            color = 'white') +
  labs(title = "Distribution of Rank")

df %>% 
  ggplot(aes(x = rank)) +
  geom_boxplot() +
  labs(title = "Boxplot of Rank")


# Bivariate Graphs ####
# GRE
df %>% 
  ggplot(aes(x = gre, y = admit, group = admit)) +
  geom_boxplot() +
  labs(title = "Boxplots of Admittance by GRE Scores")

df %>% 
  ggplot(aes(x = gre, 
             color = as.factor(admit),
             fill = as.factor(admit)
             )) +
  geom_density(alpha = 0.2) +
  labs(title = "Density plots of Admittance by GRE Scores",
       color = 'Admittance',
       fill = 'Admittance')

# GPA
df %>% 
  ggplot(aes(x = gpa, y = admit, group = admit)) +
  geom_boxplot() +
  labs(title = "Boxplots of Admittance by GPA")

df %>% 
  ggplot(aes(x = gpa, 
             color = as.factor(admit),
             fill = as.factor(admit)
  )) +
  geom_density(alpha = 0.2) +
  labs(title = "Density plots of Admittance by GPA",
       color = 'Admittance',
       fill = 'Admittance')

# Rank
df %>% 
  ggplot(aes(x = rank, 
             fill = as.factor(admit))) + 
  geom_bar(position = position_dodge(width = 0.9)) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count), group = as.factor(admit)),
    position = position_dodge(width = 0.9),
    vjust = 1.5,
    color = "white"
  ) +
  labs(
    title = "Distribution of Rank with Admittance",
    fill = "Admittance"
  )

# Modeling ####
# Linear models 
lin_1 <- glm(admit~gre, data = df)
summary(lin_1)
lin_2 <- glm(admit~gpa, data = df)
summary(lin_2)
lin_3 <- glm(admit~rank, data = df)
summary(lin_3)
lin_4 <- glm(admit~gre + gpa, data = df)
summary(lin_4)
lin_5 <- glm(admit~rank:gpa, data = df)
summary(lin_5)

# Linear model comparisons
lin_comps <- compare_performance(
  lin_1, lin_2, lin_3, lin_4, lin_5, 
  rank = TRUE)
lin_comps
lin_comps %>% plot()


# Logistic models
log_1 <- glm(admit~gre, data = df, family = 'binomial')
summary(log_1)
log_2 <- glm(admit~gpa, data = df, family = 'binomial')
summary(log_2)
log_3 <- glm(admit~rank, data = df, family = 'binomial')
summary(log_3)
log_4 <- glm(admit~rank + gpa, data = df, family = 'binomial')
summary(log_4)
log_5 <- glm(admit~rank + gpa + gre, data = df, family = 'binomial')
summary(log_5)

log_comps = compare_performance(
  log_1, log_2, log_3, log_4, log_5,
  rank = TRUE)
log_comps



