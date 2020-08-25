library(readr)
library(ggplot2)

# LOAD DATA
csse_covid_19_daily_reports_us <- read_csv("GitHub/CareyDataScience/bookcamp/data/csse_covid_19_daily_reports_us.csv")

time_series_covid19_deaths_US <- read_csv("GitHub/CareyDataScience/bookcamp/data/time_series_covid19_deaths_US.csv")


 

# Visualize 
p<-ggplot(data=csse_covid_19_daily_reports_us, aes(x= reorder(Province_State, -Deaths), y=Deaths)) +
  geom_bar(stat="identity")
# Horizontal bar plot
p + coord_flip()

p

library(ggalt) # devtools::install_github("hrbrmstr/ggalt")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")
library(tidyverse)

nord <- read_palette("nord.ase")

show_palette(nord)

nord = c("#8FBCBB",
"#88C0D0",
"#81A1C1",
"#5E81AC",
"#BF616A",
"#D08770",
"#EBCB8B",
"#A3BE8C",
"#B48EAD",
"#2E3440",
"#3B4252",
"#434C5E",
"#4C566A")


world <- map_data("world")
world <- world[world$region != "Antarctica", ]

usa = world[world$region=="USA"&world$long < 0 & world$long > -130,]



ggplot() +
  geom_cartogram(
    data = usa, map = usa,
    aes(x = long, y = lat, map_id = region),
    color = nord[[13]], fill = nord[[10]], size = 0.125
  ) +
  #coord_proj("+proj=wintri") +
  #scale_size_area(name = "Node count", max_size = 20, labels = scales::comma) +
  labs(
    x = NULL, y = NULL,
    title = "Covid Cases USA",
    subtitle = "(Using bubbles seemed appropriate for some, odd reason)",
    caption = "Johns Hopkins University"
  ) +
  geom_point(
    data = csse_covid_19_daily_reports_us[(csse_covid_19_daily_reports_us$Long_ < 0 & csse_covid_19_daily_reports_us$Long_ > -130 & !is.na(csse_covid_19_daily_reports_us$Long_)),], aes(Long_, Lat, size = Deaths), fill = nord[[7]],
    shape = 21, alpha = 2/3, stroke = 0.25, color = "#2b2b2b"
  ) +
  #coord_proj("+proj=wintri") 
  scale_size_area(name = "Deaths", max_size = 20, labels = scales::comma) +
  labs(
    x = NULL, y = NULL,
    title = "Covid Distribution in USA",
    subtitle = "By State",
    caption = "Johns Hopkins University"
  )+
  theme(panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = nord[[13]], color =nord[[13]])) +
  theme(panel.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(legend.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(panel.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(legend.key = element_rect(fill = nord[[13]], color = nord[[13]]))
