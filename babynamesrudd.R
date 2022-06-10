# load the packages we'll need -----

library(dplyr)
library(ggplot2)
library(babynames)

# get to know babynames -----

# the names of the columns:
names( babynames ) 

# look at the first few rows:
head( babynames ) 

# count some things -----

# count the number of males and females born (and registered!) in 1880 ;
# just look at the result instead of saving as a data.frame
babynames %>% 
  filter( year == 1880 ) %>%
  group_by(sex) %>%
  tally() 

# tally up the number of baby names by year and by sex, 
# save the results in a data.frame called name_counts
name_counts <- babynames %>%
  group_by(year, sex) %>%
  tally() 

# look at the first few rows of name_counts
head( name_counts )

# plot some counts -----

# here's our first (very plain!) line plot :
ggplot( data = name_counts, 
        aes( x=year, y=n, color=sex)) +
  geom_line( )

# and this is the same line plot, with labels :
ggplot( ) +
  geom_line( data = name_counts, 
             aes( x=year, y=n, color=sex) )  +
  labs( title = "Unique names over time",
        subtitle = "By sex",
        caption = "DataLab 2022")

# create one panel for females and one for males with facet_wrap :
ggplot( data = name_counts, 
        aes( x = year, 
             y = n )) +
  geom_line() +
  facet_wrap( ~sex, ncol=1 ) +
  labs( x = "Year",
        y = "Number of names",
        title = "Unique names over time",
        subtitle = "By sex", 
        caption = "DataLab 2022")

# use select to select specific columns -----
# (I don't like the `prop` column, so let's get rid of it)

babynames <- babynames %>%
  select( year, sex, name, n)

# note that prop is gone now:
head( babynames )    

# count babies, not names, in 2 different ways :

# Monae :
babynames %>% 
  group_by( year, sex ) %>%
  tally( n )

# Carter:
( babies_per_year <- babynames %>% 
    group_by(year, sex) %>%
    summarize( total = sum(n) ) )

# plot number of babies per year by sex

# with separate panels :
ggplot( data = babies_per_year, 
        aes( x=year, y=total)) +
  geom_line() +
  facet_wrap( ~sex, ncol=1 )

# or with separate colors :
ggplot( data = babies_per_year, 
        aes( x=year, y=total, color=sex)) +
  geom_line()  + 
  labs( title = "Total number of babies per year", 
        subtitle = "By sex",
        caption = "DataLab 2022")

# summarizing collapses groups -----

# summarizing once adds up the number of babies of _each sex_ per year
babynames %>% 
  group_by(year, sex) %>%             
  summarize( total = sum(n) ) 

# summarizing twice adds up the number of babies per year (sexes combined)
babynames %>% 
  group_by(year, sex) %>%             
  summarize( total = sum(n) ) %>%     
  summarize( total = sum(total)) 

# summarizing thrice adds up the total number of babies born and registered between 1880 and 2017
babynames %>% 
  group_by(year, sex) %>%             
  summarize( total = sum(n) ) %>%     
  summarize( total = sum(total)) %>%  
  summarize( total = sum(total))

# and any additional summarize calls do nothing new!
babynames %>% 
  group_by(year, sex) %>%             
  summarize( total = sum(n) ) %>%     
  summarize( total = sum(total)) %>%  
  summarize( total = sum(total)) %>%  
  summarize( total = sum(total)) 

# plotting one name's popularity over the years -----

karen <- babynames %>%
  filter( name == "Karen", sex == "F")

ggplot( data=karen, aes(x=year, y=n)) +
  geom_line() +
  labs( title = "Karens across the years",
        subtitle="God help us all",
        caption="Go get your manager")

# plotting the popularity of a few different names over the years -----

# warm-up: find popular names in 2001
babynames %>% 
  filter( year == 2001 ) %>%
  arrange( desc(n))

# find popular names from the 1950s :
babynames %>%
  filter( year >= 1950, year < 1960 ) %>% 
  group_by( sex, name ) %>% 
  summarize( total = sum(n)) %>% 
  arrange( desc(total ) )

# now explore how the 3 most popular female babynames from the 50s have changed in
# popularity over time :
boomers <- babynames %>% 
  filter( name == "Mary" | name == "Linda" | name == "Patricia",
          sex == "F" )
ggplot( data = boomers ) +
  geom_line( aes( x=year, y=n, color=name)) +
  labs( title = "Most popular female names from the 50s" ) +
  facet_wrap( ~name, ncol=1) + 
  theme( legend.position = "none" ) + 
  scale_color_manual( values = c("red", "darkolivegreen4", "hotpink") )
# scale_color_manual( values = c("limegreen","green", "lemonchiffon")) # use this instead if you want to barf :0

# everything -- and I mean everything! -- is customizable with ggplot -----

# for example, try out some different themes with
library( ggthemes )

ggplot( data = boomers ) +
  geom_line( aes( x=year, y=n, color=name)) +
  labs( title = "Most popular female names from the 50s" ) +
  facet_wrap( ~name, ncol=1) + 
  theme( legend.position = "none" ) +  # note: this can be before or after theme_fivethirtyeight() 
  theme_fivethirtyeight() 

ggplot( data = boomers ) +
  geom_line( aes( x=year, y=n, color=name)) +
  labs( title = "Most popular female names from the 50s" ) +
  facet_wrap( ~name, ncol=1) + 
  theme_clean() + 
  theme( legend.position = "none" ) # note: this has to be AFTER theme_clean() 

# get 10 most popular GenX names :

genx <- babynames %>%
  filter( year >= 1965, year <= 1981) %>%
  group_by(sex, name) %>% 
  summarize( total = sum(n) ) %>% 
  slice_max( order_by = total, n = 16) 

# and now put the most popular female names in a vector
( genx_top16_female <- genx %>% filter(sex == "F") %>% pull( name ) )

# extract the most popular female genx names from babynames :
df <- babynames %>% 
  filter( name %in% genx_top16_female, sex == "F" )

# and plot them, one panel per name :
ggplot( data = df, aes(x=year, y=n, color=name)) +
  geom_line() + 
  facet_wrap(~name) + 
  theme( legend.position = "none")

# geom_area paints under the line for a cool alternative to plain ol' geom_line:

joe <- babynames %>%
  filter(sex == 'M', name == 'Joseph') # make your own data.frame for your name!

ggplot(data = me, 
       aes(x = year,
           y = n)) +
  geom_area(alpha = 0.7,
            fill = 'blue',
            color = 'darkblue')

# or you can make lots of these (called _small multiples_)
ggplot( data = df, aes(x=year, y=n, fill=name)) +
  geom_area( alpha = 0.7 ) + 
  facet_wrap(~name) + 
  theme( legend.position = "none")