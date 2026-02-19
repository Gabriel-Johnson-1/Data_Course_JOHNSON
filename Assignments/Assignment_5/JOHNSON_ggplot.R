library(tidyverse)
df <- iris
names(df)

assignment_dir <- 'Assignments/Assignment_5/'
#### Graph one ####
p1 <- df %>% 
  ggplot(aes(x=Sepal.Length, 
             y = Petal.Length, 
             fill=Species, 
             color=Species)) + 
  geom_point() +
  geom_smooth(method='lm') +
  labs(title='Sepal Length vs Petal Length',
       subtitle='for the 3 iris species') +
  theme_minimal()
p1
ggsave(paste(assignment_dir, 'iris_fig1.png'), plot=p1)


#### Graph 2 ####
p2 <- df %>% 
  ggplot(aes(x=Petal.Width, fill=Species)) + 
  geom_density(alpha = 0.5) +
  labs(title='Distribution of Petal Widths',
       subtitle='for 3 iris species') +
  theme_minimal()
p2
ggsave(paste(assignment_dir, 'iris_fig2.png'), plot=p2)

#### Graph 3 ####
df$Sepal_Petal_Width_Ratio <- df$Petal.Width/df$Sepal.Width 
p3 <- df %>% 
  ggplot(aes(y=Sepal_Petal_Width_Ratio,
             x=Species,
             fill=Species)) +
  geom_boxplot() +
  labs(title="Sepal to Petal-Width Ratio",
       subtitle='for 3 iris species',
       y='Ratio of Sepal Width to Petal Width') 
p3
ggsave(paste(assignment_dir, 'iris_fig3.png'), plot=p3)


#### Graph 4 ####
df$sep_len_dev <- df$Sepal.Length - mean(df$Sepal.Length)
df <- df %>% 
  arrange(sep_len_dev)
df$index <- 1:nrow(df)
df %>% 
  ggplot(aes(y=sep_len_dev, x=index,fill=Species, color=Species))+
  geom_col(stat = 'identity', position='dodge', width=0.7) + 
  coord_flip() +
  scale_x_continuous(breaks = df$index) +
  theme_minimal() +
  labs(title='Sepal length deviation from the mean of all observations',
       x='Deviance from the mean') +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 

  
  
  











