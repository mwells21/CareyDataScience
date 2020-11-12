library(tidyverse)
library(readr)
library(pastecs)

setwd("~/GitHub/CareyDataScience/econometrics")

auto <- read_csv("auto.csv")

# Separate the data 
foreign = auto[auto$foreign == 0,]
non_foreign = auto[auto$foreign == 1,]

# Information 
pastecs::stat.desc(auto)
pastecs::stat.desc(foreign)
pastecs::stat.desc(non_foreign)













