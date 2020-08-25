library(readr)
library(ggplot2)
library(ggthemes)

setwd("~/GitHub/CareyDataScience")


# LOAD DATA
csse_covid_19_daily_reports_us <- read_csv("bookcamp/data/csse_covid_19_daily_reports_us.csv")

time_series_covid19_deaths_US <- read_csv("bookcamp/data/time_series_covid19_deaths_US.csv")


 

# Visualize 
p<-ggplot(data=csse_covid_19_daily_reports_us, aes(x= reorder(Province_State, -Deaths), y=Deaths)) +
  geom_bar(stat="identity")
# Horizontal bar plot
p + coord_flip()


library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(tidyverse)


# Color Scheme 
palette = c("#8FBCBB","#88C0D0","#81A1C1","#5E81AC","#BF616A",
         "#D08770","#EBCB8B","#A3BE8C","#B48EAD","#2E3440",
         "#3B4252","#434C5E","#4C566A")



library(maps)
world <- map_data("world")
world <- world[world$region != "Antarctica", ]

usa = world[world$region=="USA"&world$long < 0 & world$long > -130,]


# MAP of US - Deaths By State
ggplot() +
  geom_cartogram(
    data = usa, map = usa,
    aes(x = long, y = lat, map_id = region),
    color = palette[[13]], fill = palette[[10]], size = 0.125
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Covid Deaths USA",
    subtitle = "Johns Hopkins University"
  ) +
  geom_point(
    data = csse_covid_19_daily_reports_us[(csse_covid_19_daily_reports_us$Long_ < 0 & csse_covid_19_daily_reports_us$Long_ > -130 & !is.na(csse_covid_19_daily_reports_us$Long_)),], aes(Long_, Lat, size = Deaths), fill = palette[[6]],
    shape = 21, alpha = 2/3, stroke = 0.25, color = "#2b2b2b"
  ) +
  scale_size_area(name = "Deaths", max_size = 20, labels = scales::comma) +
  theme(panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = palette[[13]], color =palette[[13]])) +
  theme(panel.background = element_rect(fill = palette[[13]], color = palette[[13]]))+
  theme(legend.background = element_rect(fill = palette[[13]], color = palette[[13]]))+
  theme(panel.background = element_rect(fill = palette[[13]], color = palette[[13]]))+
  theme(legend.key = element_rect(fill = palette[[13]], color = palette[[13]]))




# All deaths over time Plot
library("reshape")
states = unique(time_series_covid19_deaths_US$Province_State)

# Reorganized the table 
# Probably an easier way to do this 

states_over_time = cbind(states_over_time, time_series_covid19_deaths_US[,13:ncol(time_series_covid19_deaths_US)])

# Melt Data 
tmp = melt(states_over_time,id.vars = "UID")

# Deaths is an object with columns: "UID","varible"(Every Date),"value" (The number of deaths),"state"
stateUIDs = tmp[tmp$value %in% states,]
deaths = tmp[!(tmp$value %in% states),]
deaths$state = stateUIDs$value[match(deaths$UID,stateUIDs$UID)]
deaths$variable=  mdy(as.character(deaths$variable))



# Loop sum every locations deaths in each state for every day
dates = unique(deaths$variable)
this.tmp.state = NULL
state_dates = NULL
for(i in 1:length(states)){
  this.state = states[i]
  for(j in 1:length(dates)){
    this.date = dates[j]
    this.dates.states = deaths[deaths$variable == this.date &deaths$state == this.state,]
    this.tmp.date = data.frame(state = this.state, date = this.date, deaths = sum(as.integer(this.dates.states$value)))
    if(is.null(this.tmp.state)){
      this.tmp.state = this.tmp.date
    } else {
      this.tmp.state = rbind(this.tmp.state,this.tmp.date)
    }
  }
  
  if(is.null(state_dates)){
    state_dates = this.tmp.state
  } else {
    state_dates = rbind(state_dates,this.tmp.state)
  }
  
  this.tmp.state = NULL
}


# Create colors for the highest states 
state_colors = c("red4","firebrick1","darkorange4","chocolate1","orange","darkgoldenrod","gold","yellow","lightgoldenrod",rep("gray",49))
names(state_colors) = state_dates$state[state_dates$date == "2020-08-19"][rev(order(state_dates$deaths[state_dates$date == "2020-08-19"]))]

 
# Line plot of deaths in US state over time 
ggplot(state_dates, aes(x = date, y = deaths)) + 
  geom_line(aes(color = state), size = 1) +
  scale_color_manual(values = state_colors) +
  theme_gdocs()+
  labs(
    x = NULL, y = NULL,
    title = "Covid Deaths USA",
    subtitle = "Johns Hopkins University"
  ) 





# Change in daily deaths per state 

state_dates$daily_change = NA
for( i in 1:length(dates)){
  this.date = dates[i]
  if( i == 1 ){
    this.change = 0 
  } else {
    past.date = dates[i-1]
    this.change = state_dates$deaths[state_dates$date == this.date] - state_dates$deaths[state_dates$date == past.date]
  }
  state_dates$daily_change[state_dates$date == this.date] = this.change
}

ggplot(state_dates, aes(x = date, y = daily_change)) + 
  geom_line(aes(color = state), size = 1) +
  scale_color_manual(values = state_colors) +
  theme_gdocs()+
  labs(
    x = NULL, y = NULL,
    title = "Covid Deaths USA",
    subtitle = "Johns Hopkins University"
  ) 











