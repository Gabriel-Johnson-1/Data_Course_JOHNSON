library(tidyverse)
airlines <- read.csv('airlines.csv')
airports <- read.csv('airports.csv')
jan_flights <- read.csv('jan_flights.csv')
jan_snowfall <- read.csv('Jan_snowfall.csv')

names(airlines)
names(airports)
names(jan_flights)
names(jan_snowfall)

# Combine the data sets appropriately to investigate whether departure 
# delay was correlated with snowfall amount

