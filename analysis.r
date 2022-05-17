library(tidyverse)
library(stringr)
library(ggplot2)

#incarcerationdf <- read.csv(file = url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))

#VALUES BEING FOUND 

#how much has Black male pison  pop rates changed over the past 10 yrs in Prison 2008 - 2018 per state
# // how much has the jail pop  rates chnages fro balck males, is there a corralation>? 
# where is this variable the highest? / whats state / county 
# how much has the ice pop changed for the past 10 yrs  in Prison 2008 - 2018 ? 
# where is this variable the hightest/ what state/ county ? 
# what is the avg ratio of black/non, and whatyear is it highest & what is ratio of lation/ice and what year is highest? 

# SOURCING DATA / VALUES

# BLACK MALE PRISION POP CHANGES OVER 10 YEARS PER STATE 
# AND BLACK MALE JAIL POP precent per pop CHANGES 

black_male_pop <- select(incarcerationdf, year, state, county_name, male_prison_pop, black_prison_pop)
past10bmalepop <- filter(black_male_pop, year >= "2008")

change_state <- past10bmalepop %>%
  group_by(state) %>%
  mutate(yrchange = black_prison_pop - lag(black_prison_pop))
change_state[is.na(change_state)] <- 0
years_change <- change_state %>%
  summarise(change = sum(yrchange))

# PRECENT OF MALE POP WHO IS BLACK PER STATE OVER 10 YEARS

blackmalejail_pop <- select(incarcerationdf, year, state, county_name, black_jail_pop, male_jail_pop)
past10jailpop <- filter(blackmalejail_pop, year >= "2008")
past10jailpop <- past10jailpop %>%
  mutate(popercent = black_jail_pop/male_jail_pop * 100) 


percent_avg <- select(past10jailpop, year, state, popercent)
percent_avg <- percent_avg %>%
  group_by(state) %>%
  
  
#aggregate(percent_avg$popercent, by = list(percent_avg$state, 
                                           #percent_avg$popercent), FUN = mean)


# ICE POP PRISION CHANGES OVER 10 YEARS PER STATE 

ice_pop <- select(incarcerationdf, year, state, county_name, total_jail_from_ice, latinx_jail_pop, total_jail_pop)
past10icepop <-  filter(ice_pop, year >= "2008")
icestate_change <- past10icepop %>%
  group_by(state) %>%
mutate(yrchange = total_jail_from_ice -lag(total_jail_from_ice))
  icestate_change[is.na(icestate_change)] <- 0 
years_icechange <- icestate_change %>%
    summarise(change = sum(yrchange))
  



#The first chart that you'll create and include will show the
#trend over time of your variable/topic. Think carefully about
#what you want to communicate to your user (you may have to find relevant 
#trends in the dataset first!). 
#Here are some requirements to help guide your design
#Show more than one, but fewer than ~10 lines: your graph should compare the trend of y
#our measure over time. This may mean showing the same measure 
#for different locations, or different racial groups.
#Think carefully about a meaningful comparison of locations 
#(e.g., the top 10 counties in a state, top 10 states, etc.)
#You must have clear x and y axis labels,
#The chart needs a clear title 
#You need a legend for your different line colors, and a clear legend title
#In your .Rmd file, make sure to describe why you included the chart, and what patterns emerged
