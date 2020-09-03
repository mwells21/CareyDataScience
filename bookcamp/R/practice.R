library(tidyverse)
library(ggthemes)

setwd("~/GitHub/CareyDataScience")

# LOAD DATA
csse_covid_19_daily_reports_us <- read_csv("bookcamp/data/csse_covid_19_daily_reports_us.csv")

time_series_covid19_deaths_US <- read_csv("bookcamp/data/time_series_covid19_deaths_US.csv")


x = csse_covid_19_daily_reports_us$Deaths

y = csse_covid_19_daily_reports_us$Confirmed


plot(x,y)

