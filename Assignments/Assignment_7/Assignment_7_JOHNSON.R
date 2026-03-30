library(tidyverse)
library(lubridate)
airlines <- read.csv('airlines.csv')
airports <- read.csv('airports.csv')
jan_flights <- read.csv('jan_flights.csv')
jan_snowfall <- read.csv('Jan_snowfall.csv')

names(airlines)
names(airports)
names(jan_flights)
names(jan_snowfall)

## Combine the data sets appropriately to investigate whether departure 
## delay was correlated with snowfall amount

# fixing dates
jan_flights <- jan_flights %>% 
  mutate(Date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
  select(Date, ORIGIN_AIRPORT, DEPARTURE_DELAY)
View(jan_flights)


jan_snowfall <- jan_snowfall %>% 
  mutate(Date = as.Date(Date),
         ORIGIN_AIRPORT = iata) %>% 
  select(-iata)

joint <- left_join(jan_flights, jan_snowfall, by=c("Date", "ORIGIN_AIRPORT"))

joint <- joint %>% 
  mutate(DEPARTURE_DELAY = replace_na(DEPARTURE_DELAY, 0))
names(joint)

joint %>% 
  ggplot(aes(x = snow_precip_cm, y = DEPARTURE_DELAY)) +
  geom_point()


joint <- joint %>% 
  mutate(snowed = case_when(snow_precip_cm > 0 ~ 1,
                            snow_precip_cm <= 0 ~ 0),
         delayed = case_when(DEPARTURE_DELAY > 0 ~ 1,
                             DEPARTURE_DELAY <= 0 ~ 0))

model <- lm(delayed~snowed, data=joint)
summary(model)

