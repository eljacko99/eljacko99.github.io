library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

pms <- read_csv('https://github.com/databrew/intro-to-data-science/blob/main/data/pms.csv?raw=true')

View(pms)

pms <- pms %>% 
  mutate(dow = weekdays(date_visit))

# 6. How many visits were there to Catale on May 1 2022?
pms %>% 
  filter(date_visit == '2022-05-01', health_facility == 'Catale') %>%
  tally

  
#7. How many of those were for malaria?
pms %>% 
  filter(date_visit == '2022-05-01', health_facility == 'Catale', malaria_diagnosis == 'Malaria') %>%
  tally

  
#8. Which age group has had the most malaria?

pms %>% filter(malaria_diagnosis == 'Malaria') %>% group_by(age) %>% tally %>% arrange(desc(n))
  
#9. What day of the week has the most visits?

pms %>% group_by(dow) %>% tally %>% arrange(desc(n))
  
#10. Which month has had the most malaria visits?

pms <- pms %>%
  mutate(mo = month(date_visit)) %>%
  relocate('mo', after = date_visit)

pms %>% 
  filter(malaria_diagnosis == 'Malaria') %>%
  group_by(mo) %>% 
  tally %>% arrange(desc(n))
  
#11. Which month has had the greatest percentage malaria visits?

pms %>% 
  group_by(mo, malaria_diagnosis) %>% 
  tally %>% 
  ungroup %>% 
  group_by(mo) %>% 
  mutate(total = sum(n)) %>%
  mutate(mal_perc = n/total*100) %>%
  filter(malaria_diagnosis == 'Malaria') %>% 
  arrange(desc(mal_perc))
  
  
#12. Make a variable called hour of day?

pms <- pms %>% 
  mutate(start_time = mdy_hms(start_time))

pms <- pms %>% 
  mutate(hour_of_day = hour(start_time))

View(pms)
  
#13. Which hour of day has the most visits?
x <- pms %>% 
  group_by(hour_of_day) %>% 
  tally

ggplot(data = x, aes(x = hour_of_day, y = n)) + 
  geom_point() + 
  geom_line() + 
  geom_area(fill = "pink")
  
#14. What do you think the function mdy_hms is/does?
  
#15. Look up the documentation for mdy_hms.

#16. Use mdy_hms to create a new variable in pms named date_time based on the variable start_time.

#17. Use the hour function to create a variable named hour_of_day from the date_time variable. This should be the hour of the day.

#18. Get the total number of malaria cases diagnoses by hour of day.

y <- pms %>%
  filter(malaria_diagnosis == 'Malaria') %>%
  group_by(hour_of_day) %>% 
  tally

#19. Visualize the total number of malaria cases diagnoses by hour of day.

ggplot(data = y, aes(x = hour_of_day, y = n)) + 
  geom_point() + 
  geom_line() + 
  geom_area(fill = "pink")


#20. Visualize the total number of malaria cases diagnoses by hour of day, but separated by day.

df <- pms %>% filter(malaria_diagnosis == 'Malaria') %>% group_by(hour_of_day, dow) %>% tally

View(df)

ggplot(data = df, aes(x = hour_of_day, y = n, fill = dow)) + 
  geom_area(alpha = 0.5) + 
  facet_wrap(~ dow) + 
  labs(x = 'Hour of day', y = 'Number of bad air visits') +
  theme_clean() +
  theme(legend.position = 'none') 
 













