library(dplyr)
library(ggplot2)
library(babynames)
library(ggthemes)

names(babynames)
head(babynames)
babynames %>% filter(year == 1880) %>% group_by(sex) %>% tally()

# tally up number of baby names by year and by sex

df <- babynames %>% group_by(year, sex) %>% tally()
head(df)

ggplot(data = df, aes(x = year, y = n, color = sex)) + geom_line()

#same thing but with captions
ggplot() + geom_line(data = df, aes(x = year, y =n, color = sex)) + 
  labs(title = "unique names over time", subtitle = "By sex", caption = "DataLab 2022")

# create one panel for females and one for males with facet_wrap
ggplot(data = df, aes(x = year, y = n)) + geom_line() + facet_wrap(~sex)

# same thing but slightly different and with titles
ggplot(data = df, aes(x = year, y = n)) + geom_line() + facet_wrap(~sex, ncol=1) + labs(title = "Unique names over time", subtitle = "By sex", caption = "DataLab 2022")

ggplot(data = df, aes(x = year, y = n)) + geom_line() + facet_wrap(~sex, ncol=1) + labs(x = "Year", y = "Number of names", title = "Unique names over time", subtitle = "By sex", caption = "DataLab 2022")


# I don't like the 'prop' column, so let's get rid of it

newdf <- babynames %>% select(year, sex, name, n)
head(newdf)

#Total number of babies of each sex per year

#Monae:
babies_per_year <- babynames %>% group_by(year, sex) %>% tally(n)
head(babies_per_year)

#OR

#Carter:
also_babies_per_year <- babynames %>% group_by(year, sex) %>% summarise(total = sum(n))
head(also_babies_per_year)


# plot number of babies per year by sex
ggplot(data = also_babies_per_year, aes(x = year, y = total)) + geom_line() + facet_wrap(~sex, ncol = 1)



newbabynames <- babynames %>% group_by(year, sex) %>% summarise(total = sum(n)) %>% summarize(total = sum(total))
head(newbabynames)

karen <- babynames %>% filter(name == "Karen", sex == 'F')
head(karen)

ggplot(data = karen, aes(x = year, y = n)) + geom_line()

ggplot(data = karen, aes(x = year, y = prop)) + geom_line()

ruddkaren <- babynames %>% filter(name == "Karen")
ggplot(data = ruddkaren, aes(x = year, y = n)) + geom_line() + labs(title = "Karens across the years", subtitle = "God help us", caption = "Go get your manager")

ninetyninenames <- babynames %>% filter(year == 1999)%>% group_by(sex) %>% arrange(desc(n))
head(ninetyninenames)

boomers <- babynames %>% filter(year >= 1950, year <1960) %>% group_by(sex, name) %>% summarise(total = sum(n))%>% arrange(desc(total))
head(boomers)

threefemalenames <- babynames %>% filter(name == "Mary" | name == "Linda" | name == "Patricia", sex == 'F') 
ggplot(data = threefemalenames, aes(x = year, y = n, color = name)) + geom_line() + labs(title = "Most popular female names from 1950 - 1959")

ggplot(data = threefemalenames, aes(x = year, y = n, color = name)) + 
  geom_line() + 
  facet_wrap(~name, ncol=1) + 
  labs(title = "Most popular female names from 1950 - 1959") + 
  theme(legend.position = "none") +
  scale_color_manual(values = c("red", "darkolivegreen3", "hotpink")) +
  theme(legend.position = "none") +
  theme_clean()


genx <- babynames %>% 
  filter(year >= 1965, year <= 1981) %>% 
  group_by(sex, name) %>% 
  summarize(total = sum(n)) %>%
  slice_max(order_by = total, n = 5)

View(genx)

#This line puts the top female names from  '65 to '81 in a vector

genx_top5_female <- genx %>% filter(sex == "F") %>% pull(name)

dfnew <- babynames %>%
  filter(name %in% genx_top5_female, sex == "F")

ggplot(data = dfnew, aes(x = year, y = n, color = name)) + geom_line() + facet_wrap(~name)


genz <- babynames %>% 
  filter(year >= 1996, year <= 2011) %>%
  group_by(sex, name) %>%
  summarize(total = sum(n)) %>%
  slice_max(order_by = total, n = 5)

View(genz)

genznames <- genz %>% pull(name)

dfnewnew <- babynames %>%
  filter(name %in% genznames)

View(dfnewnew)

ggplot(data = dfnewnew, aes(x = year, y = n, color = name)) + 
  geom_line() + 
  facet_wrap(~name) +
  labs(title = "Popularity of names that were popular for babies from 1996 to 2011")

me <- babynames %>% filter(sex =='M', name == "Jack")

View(me)

ggplot(data = me, aes(x = year, y = n)) + 
  geom_area(alpha = 0.7, fill = 'blue', color = 'darkblue')








