library(dplyr)
library(ggplot2)
library(readr)
library(tidytext)
library(sentimentr)
library(wordcloud)
library(lubridate)
library(ggthemes)

elon <- read_csv("https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/elon_musk.csv")
View(elon)

names(elon)

elontime <- elon %>% 
  mutate(hod = hour(date)) %>%
  group_by(hod) %>% 
  tally

ggplot(data = elontime, aes(x = hod, y = n)) + 
  geom_line() + 
  labs(title = "Elon Musk's tweets", subtitle = "2010 to 2020") +
  theme_clean()

range(elon$date)








