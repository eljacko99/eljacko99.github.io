x <- 3+3
x
#7
bananaseaten <- 23*365*(1/7)

icseaten <- 4*23
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gsheet")
install.packages("readr")
install.packages("leaflet")
Bananaicsratio <- bananaseaten/icseaten
Bananaicsratio 

library(dplyr) 
library(leaflet)
library(gsheet)

c(1, 4, 7, 9)  %>%  mean

url <- 'https://docs.google.com/spreadsheets/d/1xoecVY2roNzS2gpt8UnvhGhCxrocXjJMpji9eUgiDMw/edit?usp=sharing'
df <- gsheet2tbl(url)
leaflet()


leaflet() %>% addTiles()

leaflet() %>% addTiles() %>% addMarkers(data = df)

leaflet() %>% addProviderTiles(providers$Esri.WorldImagery)

leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>% addCircleMarkers(data = df)


