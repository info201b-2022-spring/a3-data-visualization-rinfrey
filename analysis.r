library(tidyverse)
library(stringr)
library(ggplot2)
library(fmsb)
library(maps)
library(ggmap)

#incarcerationdf <- read.csv(file = url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))



# SOURCING DATA / VALUES


#POPULATIONS IN LA COUNTY JAIL OVER 10 YEARS 2008 - 2018

blackmalejail_pop <- select(incarcerationdf, year, state, county_name, black_pop_15to64, total_pop_15to64)
past10jailpop <- filter(blackmalejail_pop, year >= "2008")
past10jailpop <- past10jailpop %>%
  mutate(black_popercent = black_pop_15to64/total_pop_15to64 * 100) 


latino_pop <- select(incarcerationdf, year, state, county_name, latinx_pop_15to64, white_pop_15to64, total_pop_15to64)
latinopast_10 <- filter(latino_pop, year >= "2008")
latinopast_10 <- latinopast_10 %>%
  mutate(latino_poppercent = latinx_pop_15to64/total_pop_15to64 * 100)

white_pop <- latino_pop
white_pop <- filter(white_pop, year >= "2008")
white_pop <- white_pop %>%
  mutate(white_poppercent = white_pop_15to64/total_pop_15to64 * 100)

    
black_latino_poppercent <- data.frame(latinopast_10$year, latinopast_10$state, 
                                      latinopast_10$county_name, latinopast_10$latino_poppercent, 
                                      past10jailpop$black_popercent, past10jailpop$total_pop_15to64, 
                                      white_pop$white_poppercent)
                                  

black_latino_poppercent = rename(black_latino_poppercent, year = latinopast_10.year, 
                                 county_name = latinopast_10.county_name, 
                                 state = latinopast_10.state, 
                                 latino_pop_percent = latinopast_10.latino_poppercent, 
                                 black_pop_percent = past10jailpop.black_popercent, 
                                 total_jail_pop = past10jailpop.total_pop_15to64, 
                                 white_pop_percent = white_pop.white_poppercent)

black_latino_poppercent <- black_latino_poppercent %>%
  group_by(year) %>%
  slice_max(total_jail_pop, n = 1) 

chart_1 <- black_latino_poppercent %>%
  select(year, latino_pop_percent, black_pop_percent, white_pop_percent) %>%
  gather(key = population_percentages, value = population, - year)

chart_1 <- ggplot(chart_1) + 
  geom_col(mapping = aes(x = year, y = population, fill = population_percentages), position =  position_dodge(0.7), width = 1) +
  scale_fill_brewer(palette = "Accent")


# HOW HAS BLACK JAIL POPULATION PERCENT CHANGED OVER THE YEARS, PULLING FROM COUNTYS WITH THE MAX CHANGE 

percent_avg <- select(past10jailpop, year, state, county_name, black_popercent)
percent_avg <- percent_avg %>%
  group_by(state) %>%
  mutate(change_over_time = black_popercent -lag(black_popercent))
popchange_over_time <- percent_avg 
  lapply(popchange_over_time, function(x) x[is.finite(x)])
popchange_over_time <- popchange_over_time %>%
  group_by(year, state) %>%
slice_max(change_over_time, n = 1)

table_pop_change <- popchange_over_time %>%
  group_by(state) %>%
  summarise(black_pop_change = mean(change_over_time))



# WHAT PRECENT  OF FEMALE PRISON  ADMITS ARE BLACK WOMEN? PER STATE 
# AND 
# WHAT PRECENT OF MALE PRISON ADMITS ARE BLACK MEN 

female_prison_adm_percent <- select(incarcerationdf, state, year, black_female_prison_adm, total_prison_adm)

female_prison_adm_percent <- female_prison_adm_percent %>%
  group_by(state) %>%
 mutate(female_pop_percent = black_female_prison_adm/ total_prison_adm * 100)
female_prison_adm_percent <- na.exclude(female_prison_adm_percent)
female_prison_adm_percent <- female_prison_adm_percent %>%
  group_by(state)%>%
  summarise(favg = mean(female_pop_percent))

male_prison_adm_percent <- select(incarcerationdf, year, state, black_male_prison_adm, total_prison_adm )


male_prison_adm_percent <- male_prison_adm_percent %>%
  group_by(state) %>%
  mutate(male_pop_percent = black_male_prison_adm/total_prison_adm * 100 )
male_prison_adm_percent <- na.exclude(male_prison_adm_percent)
male_prison_adm_percent <- male_prison_adm_percent %>%
  group_by(state) %>%
  summarise(mavg = mean(male_pop_percent))

male_female_avg_prison_admits <- merge(female_prison_adm_percent, male_prison_adm_percent)
male_female_avg_prison_admits <- male_female_avg_prison_admits %>%
  arrange(desc(mavg))
male_female_avg_prison_admits <- male_female_avg_prison_admits %>%
  slice_max(mavg, n = 10)



chart_2 <- male_female_avg_prison_admits %>%
  select(state, favg, mavg) %>%
  gather(male_and_female_prison_admit_avg, value = avg, - state)

chart_2 <- ggplot(chart_2) +
  geom_col(mapping = aes(x = state, y = avg, fill = male_and_female_prison_admit_avg), position = position_dodge(0.7), width = 1) +
  scale_fill_brewer(palette = "Accent")


# WHAT PRECENT OF A STATE'S PRISON POP IS BLACK in 2018?

black_prison_pop <- select(incarcerationdf, state, year, total_prison_pop, black_prison_pop)
black_prison_pop <- filter(black_prison_pop, year == "2016")
black_prison_pop <- na.exclude(black_prison_pop)
black_prison_pop <- black_prison_pop %>%
  mutate(black_prison_percent = black_prison_pop/total_prison_pop * 100)
black_prison_pop <- black_prison_pop %>%
  group_by(state) %>%
  summarise(black_prison_percentage = sum(black_prison_percent))

black_prison_pop <- black_prison_pop %>%
  mutate(State = state.name[match(state, state.abb)]) %>%
  mutate(state = tolower(State))


state_shape <- map_data("state") %>%
  rename(state = region) %>%
  full_join(black_prison_pop, by = "state")

chart_3 <- ggplot(state_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_prison_percentage),
    color = "white",
    size = .1) + 
  coord_map() + 
labs(fill = " Black population prison percentage") +
theme_bw()+
  theme(axis.line = element_blank(),        
        axis.text = element_blank(),       
        axis.ticks = element_blank(),       
        plot.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank()      
) 



