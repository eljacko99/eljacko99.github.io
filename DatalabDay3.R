library(ggplot2)
library(dplyr)
library(gsheet)
library(readr)
people <- read_csv('http://datarain.global/data/survey.csv')

ggplot(data = people) + labs(title = 'polishing coding', subtitle = 'June 2 2022') + geom_histogram(aes(x = happiness))