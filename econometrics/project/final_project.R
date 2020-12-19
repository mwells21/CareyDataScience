setwd("~/GitHub/CareyDataScience/econometrics/project")

library(tidyverse)
library(maps)
library(lubridate)


# Load lock down data 
state_orders <- read_csv("state_orders.csv")
state_orders = na.omit(state_orders)

# State location 
us_states <- map_data("state")

# Add lock down info to the us_states data frame
us_states$lockdown = state_orders$`Effective Date`[match(us_states$region, tolower(state_orders$State))]
us_states$Month = state_orders$Month[match(us_states$region, tolower(state_orders$State))]
us_states$Day = state_orders$Day[match(us_states$region, tolower(state_orders$State))]


# ---- US plot ---- 
p <- ggplot(
  data = us_states,
  mapping = aes(
    x = long,
    y = lat,
    fill = Month,
    group = group)
)+scale_fill_brewer(palette="Set1") 

p + geom_polygon()+
  theme(axis.title = element_blank(),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
  )+ 
  ggtitle( "Covid Stay-at-Home Orders")




#---- Lockdown duration ---- 
state_orders$Start = mdy(state_orders$Start)
state_orders$End = mdy(state_orders$End)

p1 = ggplot(data = state_orders, aes(x = State, ymin = Start, ymax = End ))+
  geom_linerange()+
  geom_point(data = state_orders, aes(x = State, y = Start) )+
  geom_point(data = state_orders, aes(x = State, y = End) )+
  coord_flip()+
  theme(axis.title = element_blank())+
  ggtitle( "Initial Covid Stay-at-Home Order Duration")

p2 = ggplot(data = state_orders, aes(x = State, ymin = 0, ymax = Duration ))+
  geom_linerange()+
  geom_point(data = state_orders, aes(x = State, y = 0))+
  geom_point(data = state_orders, aes(x = State, y = Duration) )+
  geom_abline(intercept = min(state_orders$Duration),slope = 0, color = "red")+
  geom_abline(intercept = -min(state_orders$Duration),slope = 0, color = "red")+
  ylim(-30, 300)+
  theme(axis.title = element_blank())+
  #scale_y_continuous(expand=c(.5,-.5),)+
  coord_flip()+
  ggtitle( "Adjusted Initial Covid Stay-at-Home Order Duration")

plot_grid(p1, p2, labels = "AUTO")
























