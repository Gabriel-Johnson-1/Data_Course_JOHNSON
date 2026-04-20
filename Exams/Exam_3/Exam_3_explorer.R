library(tidyverse)
library(skimr)
library(janitor)
library(broom)

# Question 1: ####
df <- read.csv('FacultySalaries_1995.csv')
summary(df)

longer <- df %>% 
  pivot_longer(cols = c(AvgFullProfSalary, AvgAssocProfSalary, AvgAssistProfSalary),
               names_to = 'Rank',
               values_to = 'Salary'
               ) %>% 
  mutate(Rank = case_when(Rank == 'AvgFullProfSalary' ~ 'Full',
                          Rank == 'AvgAssocProfSalary' ~ 'Assoc',
                          Rank == 'AvgAssistProfSalary' ~ 'Assist')) %>% 
  filter(Tier != 'VIIB')

longer %>% 
  ggplot(aes(x = Rank, y = Salary, fill=Rank)) +
  facet_wrap(~Tier) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Question 2. ####
longer %>%
  aov(Salary ~ State + Tier + Rank, data = .) %>%
  summary()
  
  
  
# Question 3. ####
df <- read.csv('Juniper_Oils.csv')
chemicals <- c("alpha.pinene","para.cymene","alpha.terpineol",
               "cedr.9.ene","alpha.cedrene","beta.cedrene",
               "cis.thujopsene","alpha.himachalene",
               "beta.chamigrene","cuparene","compound.1",
               "alpha.chamigrene","widdrol","cedrol",
               "beta.acorenol","alpha.acorenol","gamma.eudesmol",
               "beta.eudesmol","alpha.eudesmol","cedr.8.en.13.ol",
               "cedr.8.en.15.ol","compound.2","thujopsenal")

long <- df %>% 
  pivot_longer(cols = chemicals,
               names_to = 'ChemicalID',
               values_to = 'Concentration')


# Question 4 ####
long %>% 
  ggplot(aes(x = YearsSinceBurn, y = Concentration)) +
  facet_wrap(~ChemicalID, scales = 'free_y') +
  geom_smooth() +
  theme_minimal()




# Question 5 ####
model <- glm(Concentration ~ ChemicalID * YearsSinceBurn, data = long)
tidy_model <- tidy(model)
significant_terms <- tidy_model %>%
  filter(p.value < 0.05)


significant_terms
