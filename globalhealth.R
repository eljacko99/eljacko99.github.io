library(readr)
library(dplyr)
library(gapminder)
gm <- gapminder

#1. How many rows are in the dataset?
nrow(gm)

#2. How many columns are in the dataset? 
ncol(gm)

#3. What are the names of the columns?
names(gm)

#4. What is the oldest year in the dataset?
min(gm$year)

#5. What is the country/year with the greatest population in the dataset?
max(gm$pop)
# OR
gm %>% filter(pop == max(gm$pop))

#6. Get the average GDP per capita for each continent in 1952.
gm %>% filter(year == 1952) %>% group_by(continent) %>% summarize(avg_gdp = mean(gdpPercap))

#7. Get the average GDP per capita for each continent for the most recent year in the dataset.
gm %>% filter(year == max(gm$year)) %>% group_by(continent) %>% summarize(avg_gdp = mean(gdpPercap))

# Special bonus Harrison question

gm %>% filter(year == max(gm$year)) %>% summarise(avg = mean(gdpPercap))

# Weighted gdp per capita
gm %>% filter(year == min(year)) %>% summarise(wm = weighted.mean(gdpPercap, w = pop))

#8. Average GDP is a bit misleading, since it does not take into account the relative size (in population) of the different countries (ie, China is a lot bigger than Cambodia). Look up the function weighted.mean. Use it to get the average life expectancy by continent for the most recent year in the dataset, weighted by population.

lifeexp <- gm %>% 
  filter(year == max(year)) %>% 
  group_by(continent) %>% 
  summarize(le = weighted.mean(lifeExp, w = pop))

#9. Make a barplot of the above table (ie, average life expectancy by continent, weighted by population).

ggplot(data = lifeexp) + geom_col(aes(x = continent, y = le))

#10. 

ggplot(data = gm) + 
  geom_point(aes(x = country, y = gdpPercap)) + 
  theme(axis.text.x = element_text(angle = 90))


#11 

ggplot(data = gm %>% filter(year == max(year))) + 
  geom_point(aes(x = country, y = gdpPercap)) + 
  theme(axis.text.x = element_text(angle = 90))

#12

ggplot(data = gm %>% filter(year == 1972)) + 
  geom_point(aes(y = gdpPercap, x = lifeExp)) + 
  geom_smooth(aes(y = gdpPercap, x = lifeExp))

#13. Make the same plot as above, but for the most recent year in the data.

ggplot(data = gm %>% filter(year == max(year))) + 
  geom_point(aes(y = gdpPercap, x = lifeExp)) + 
  geom_smooth(aes(y = gdpPercap, x = lifeExp))

#14. Make the same plot as the above, but have the size of the points reflect the population.

ggplot(data = gm %>% filter(year == max(year))) + 
  geom_point(aes(y = gdpPercap, x = lifeExp, size = pop)) + 
  geom_smooth(aes(y = gdpPercap, x = lifeExp))

#15. Make the same plot as the above, but have the color of the points reflect the continent.

ggplot(data = gm %>% filter(year == max(year))) + 
  geom_point(aes(y = gdpPercap, x = lifeExp, size = pop, color = continent)) + 
  geom_smooth(aes(y = gdpPercap, x = lifeExp))

#16. Filter the data down to just the most recent year in the data, and make a histogram (geom_histogram) showing the distribution of GDP per capita.

ggplot(data = gm %>% filter(year == max(year))) + geom_histogram(aes(x = gdpPercap))

#17. Get the average GDP per capita for each continent/year, weighted by the population of each country.

gm %<% 


#18. Using the data created above, make a plot in which the x-axis is year, the y-axis is (weighted) average GDP per capita, and the color of the lines reflects the content.

#19. Make the same plot as the above, but facet the plot by continent.

#20. Make the same plot as the above, but remove the coloring by continent.



#21. Make a plot showing France’s population over time.

#22. Make a plot showing all European countries’ population over time, with color reflecting the name of the country.

#23. Create a variable called status. If GDP per capita is over 20,000, this should be “rich”; if between 5,000 and 20,000, this should be “middle”; if this is less than 5,000, this should be “poor”.

#24. Create an object with the number of rich countries per year.

#25. Create an object with the percentage of countries that were rich each year.



#26. Create a plot showing the percentage of countries which were rich each year.

#27. Create an object with the number of people living in poor countries each year.

#28. Create a chart showing the number of people living in rich, medium, and poor countries per year (line chart, coloring by status).

#29. Create a chart showing the life expectancy in Somalia over time.

#30. Create a chart showing GDP per capita in Somalia over time.



#31. Create a histogram of life expectancy for the most recent year in the data. Facet this chart by continent.

#32. Create a barchart showing average continent-level GDP over time, weighted for population, with one bar for each year, stacked bars with the color of the bars indicating continent (geom_bar(position = 'stack')).

#33. Create the same chart as above, but with bars side-by-side (geom_bar(position = 'dodge'))

#34. Generate 3-5 more charts / tables that show interesting things about the data.

#35. Make the above charts as aesthetically pleasing as possible.



































  
  