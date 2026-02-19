#### Task I ####
# Read in the 'cleaned_covid_data.csv' into a dataframe

df <- read.csv('cleaned_covid_data.csv')
head(df, 3)
names(df)


#### Task II ####
# Subset data to show states that begin with "A" and save it
# as an object called 'A_states'

A_states <- df %>% 
  filter(grepl('^A', Province_State))
head(A_states)
unique(A_states$Province_State)


#### Task III ####
## Create a plot of that subset showing Deaths over time,
## with a separate facet for each state.
# Create a scatter plot
# Add loess curves without error bars
# Keep scales 'free' in each facet

A_states %>% 
  ggplot(aes(x = as.Date(Last_Update), y = Deaths)) +
  facet_wrap(~Province_State, scales='free') +
  geom_point() +
  geom_smooth(se=FALSE)

#### Task IV ####
## Find the “peak” of Case_Fatality_Ratio for 
## each state and save this as a new data frame 
## object called state_max_fatality_rate.
# Looking for a dataframe with 2 columns
# * “Province_State”
# * “Maximum_Fatality_Ratio”
# * Arrange the new data frame in descending order by Maximum_Fatality_Ratio

state_max_fatality_rate <- na.omit(df) %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)

#### Task V ####
## Use that new data frame from task IV to create another plot.
# X-axis is Province_State
# Y-axis is Maximum_Fatality_Ratio
# bar plot
# x-axis arranged in descending order, just like the data frame 
# (make it a factor to accomplish this)
# X-axis labels turned to 90 deg to be readable
View(state_max_fatality_rate)
state_max_fatality_rate %>% 
  ggplot(aes(x = factor(Province_State, levels = Province_State), 
             y = Maximum_Fatality_Ratio)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

#### Task VI ####
# Using the FULL data set, plot cumulative deaths 
# for the entire US over time

df %>% 
  group_by(Last_Update) %>% 
  summarize(daily_deaths = sum(Deaths), .groups = 'drop') %>%
  arrange(Last_Update) %>% 
  mutate(cum_deaths = cumsum(daily_deaths)) %>% 
  ggplot(aes(x = as.Date(Last_Update), y = cum_deaths)) +
  geom_point() +
  geom_smooth()













