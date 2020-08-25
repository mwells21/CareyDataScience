library(readr)
library(ggplot2)


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
library(maps)

# Color Scheme 
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


# MAP of US - Deaths By State
ggplot() +
  geom_cartogram(
    data = usa, map = usa,
    aes(x = long, y = lat, map_id = region),
    color = nord[[13]], fill = nord[[10]], size = 0.125
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Covid Deaths USA",
    subtitle = "Johns Hopkins University"
  ) +
  geom_point(
    data = csse_covid_19_daily_reports_us[(csse_covid_19_daily_reports_us$Long_ < 0 & csse_covid_19_daily_reports_us$Long_ > -130 & !is.na(csse_covid_19_daily_reports_us$Long_)),], aes(Long_, Lat, size = Deaths), fill = nord[[6]],
    shape = 21, alpha = 2/3, stroke = 0.25, color = "#2b2b2b"
  ) +
  scale_size_area(name = "Deaths", max_size = 20, labels = scales::comma) +
  theme(panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = nord[[13]], color =nord[[13]])) +
  theme(panel.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(legend.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(panel.background = element_rect(fill = nord[[13]], color = nord[[13]]))+
  theme(legend.key = element_rect(fill = nord[[13]], color = nord[[13]]))




# Time Plot