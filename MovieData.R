library(readr)
library(ggplot2)
library(dplyr)

movies <- read_csv('https://raw.githubusercontent.com/ericmkeen/capstone/master/movies.csv')

View(movies)

# 1. Are action movies getting longer or shorter as time goes on?


# 2. Which genre tends to generate the most revenue?


# 3. Is there a trend between IMDB ratings and runtime?

ggplot(data = movies, aes(y = rating_imdb, x = runtime)) + 
  geom_jitter(alpha = 0.3, width = 0) + 
  geom_smooth()


ggplot(data = movies, aes(y = rating_meta, x = runtime)) + 
  geom_point() + 
  geom_smooth()


# 4. Does Peter Jackson always make super long movies?

peterjackson <- movies %>% filter(director == "Peter Jackson")

ggplot(data = movies, aes(x = runtime, color )) + geom_histogram()
mean(peterjackson$runtime)
mean(movies$runtime)
sd(movies$runtime)