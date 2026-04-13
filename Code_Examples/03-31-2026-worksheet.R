## Build a model to predict city as a function of displ
## mpg dataset
library(tidyverse)
library(easystats)
df <- mpg

df %>% 
  ggplot(aes(x = displ, y = cty)) +
  geom_point() +
  geom_smooth(method = 'lm')

model <- glm(cty ~ displ, data=df)
summary(model)
report(model)
performance(model)
str(df)


preds <- predict(model, df)

model2 <- glm(cty ~ displ + cyl, data=df)
model3 <- glm(cty ~ displ + cyl + displ:cyl, data=df)
compare_models(model, model2, model3) %>% plot()
compare_performance(model, model2, model3)

model4 <- glm(cty ~ displ*cyl*year, data=df)
summary(model4)


## predict cty using 3 models and compare the results 

mod1 <- glm(cty ~ displ + year + cyl, data = df)
mod2 <- glm(cty ~ displ*cyl + year, data = df)
mod3 <- glm(cty ~ class + cyl + trans, data=df)
df$mod1 <- mod1$fitted.values
df$mod2 <- mod2$fitted.values
df$mod3 <- mod3$fitted.values

compare_performance(mod1, mod2, mod3) %>% plot()

plot(df$mod1, df$cty)
plot(df$mod2, df$cty)
plot(df$mod3, df$cty)






