library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidytext)
library(sentimentr)
library(lubridate)


# Crash data
crash <- read_csv('https://github.com/databrew/intro-to-data-science/blob/main/data/crash.csv?raw=true')

# Number of rows and columns
ncol(crash)
nrow(crash)

# Names of variables
names(crash)

# Glance at first few rows
head(crash)


cby <- crash %>% 
  mutate(year = year(Date)) %>% 
  group_by(year) %>% tally()

ggplot(data = cby) + geom_point(aes(x = year, y = n)) 

dbd

dbd <- crash %>% 
  mutate(dow = weekdays(Date)) %>% 
  group_by(dow) %>% 
  summarise(total_deaths = sum(crash$`Total fatalities`)) %>%  
  arrange(desc(total_deaths)) %>%
  tally


recent <- crash %>% 
  filter(Date >= '2015-02-13') %>%
  mutate(dow = weekdays(Date)) %>% 
  group_by(dow) %>% 
  tally


# Make a dataframe that contains only crashes where lightning was mentioned in the circumstances variable.
# Make a boolean variable to see if lightning is mentioned.
lightning <- crash %>% 
  mutate(relampago = grepl("lightning|Lightning|lightening|Lightening", Circumstances))

lightning %>% filter(relampago) %>% tally

# Create a dataframe containing only those crashes that were lighting-related.

lightning_related_crashes <- lightning %>% filter(relampago)

View(lightning_related_crashes)

# Drunk pilots

FUI <- crash %>% 
  mutate(drug = grepl('alcohol|drugs|bac|Substances|intoxicat|drink|influence', Circumstances, ignore.case = TRUE)) %>% filter(drug)

#How many plane crashes in the US in the late 2010s?

crashus <- crash %>% 
  mutate(year = year(Date)) %>% 
  filter(year >= 2010, Country == "United States of America")

View(crashus)

ggplot(data = crash, aes(x = crash$`Crew fatalities`, y = crash$`PAX fatalities`, color = Region)) + 
  geom_jitter(alpha = 0.2) + 
  geom_smooth() +
  theme_fivethirtyeight() +
  facet_wrap(~Region) +
  theme(legend.position = 'none')


survey <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1W9eGIihIHppys3LZe5FNbUuaIi_tfdscIq521lidRBU/edit?usp=sharing')

nrow(survey)
ncol(survey)

survey <- survey %>% 
  mutate(dob = as.Date(dob, "%m/%d/%Y"))

# Create a new variable for each individual 
# showing the number 

survey <- survey %>% 
  mutate(wordiness = nchar(feeling))

View(survey)

survey$dob
View(survey)

ggplot(survey) + geom_jitter(aes(x = gender, y = wordiness ))

ggplot(survey) + geom_density(aes(x = gender, y = wordiness ))

ggplot(survey, aes(x = gender, y = wordiness)) + 
  geom_jitter() + 
  geom_violin(alpha = 0.3, fill = 'darkorange')

survey <- survey %>% 
  mutate(age = as.numeric(Sys.Date() - dob)/(365.25))
View(survey)




































