library(readr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(lubridate)
library(ggthemes)
ghg <- read_csv('https://raw.githubusercontent.com/ericmkeen/capstone/master/co2.csv')

# Greenhouse gas emissions by country-year-sector, 1990 - 2018
sectors <- read_csv('https://raw.githubusercontent.com/ericmkeen/capstone/master/co2_sectors.csv')

#1. present-day patterns in GHG emissions (which regions/countries/economic sectors are the biggest emitters? Which emit the least?)
ghg$co2<- ghg$`Annual CO2 emissions (zero filled)`

df <- ghg %>% 
  filter(Entity != "World" &
           Entity != "High-income countries" &
           Entity != "Upper-middle-income countries" &
           Entity != "Lower-middle-income countries" &
           Entity != "North America (excl. USA)" &
           Entity != "Europe (excl. EU-28)" &
           Entity != "Europe (excl. EU-27)" &
           Entity != "Asia (excl. China & India)" &
           Entity != "European Union (27)" &
           Entity != "European Union (28)" &
           Entity != "Europe" &
           Entity != "Asia" &
           Entity != "North America" &
           Entity != "Africa" &
           Entity != "Oceania" &
           Entity != "Low-income countries") %>%
  group_by(Entity, Year) %>%
  summarize(totalCO2 = sum(co2)) %>%
  arrange(desc(totalCO2))

View(df)



ggplot(data = ghg) + geom_line(aes(x = Year, y = ghg$`Annual CO2 emissions (zero filled)`, color = Entity)) 

df2 <- head(df)

View(sectors$entity)

ggplot(data = df2, aes(x = Entity, y = totalCO2, fill = Entity)) + 
  geom_bar(stat = "identity") 

ggplot(data = df3, aes(x = Entity, y = totalCO2, fill = Entity)) + 
  geom_bar(stat = "identity") 


top_five_polluters <- df2$Entity

head(top_five_polluters)


## Emissions by region

regions <- ghg %>% filter(Entity == "World" |
                            Entity == "High-income countries" |
                            Entity == "Upper-middle-income countries" |
                            Entity == "Lower-middle-income countries" |
                            Entity == "North America (excl. USA)" |
                            Entity == "Europe (excl. EU-28)" |
                            Entity == "Europe (excl. EU-27)" |
                            Entity == "Asia (excl. China & India)" |
                            Entity == "European Union (27)" |
                            Entity == "European Union (28)" |
                            Entity == "Europe" |
                            Entity == "Asia" |
                            Entity == "North America" |
                            Entity == "Africa" |
                            Entity == "Oceania" |
                            Entity == "Low-income countries")

View(regions)

ggplot(data = regions) + geom_line(aes(x = Year, y = regions$`Annual CO2 emissions (zero filled)`, color = Entity))

region_by_continent <- ghg %>% filter(Entity == "World" |
                               Entity == "Europe (excl. EU-28)" |
                               Entity == "Europe (excl. EU-27)" |
                               Entity == "Asia (excl. China & India)" |
                               Entity == "European Union (27)" |
                               Entity == "European Union (28)" |
                               Entity == "Europe" |
                               Entity == "Asia" |
                               Entity == "North America" |
                               Entity == "Africa" |
                               Entity == "Oceania" )

region_by_income <- ghg %>% filter(Entity == "High-income countries" |
                                     Entity == "Upper-middle-income countries" |
                                     Entity == "Lower-middle-income countries" |
                                     Entity == "Low-income countries")




#region_america_by_income 

#region_asia_by_income

#region_europe_by_income



ggplot(data = region_by_continent) + 
  geom_line(aes(x = Year, y = region_by_continent$`Annual CO2 emissions (zero filled)`, color = Entity)) + 
  labs(title ="CO2 emissions by continent from 1750 to 2020.", y = "Annual CO2 emissions")

ggplot(data = region_by_income) + 
  geom_line(aes(x = Year, y = region_by_income$`Annual CO2 emissions (zero filled)`, color = Entity)) + 
  labs(title ="CO2 emissions by level of income from 1750 to 2020.", y = "Annual CO2 emissions")















