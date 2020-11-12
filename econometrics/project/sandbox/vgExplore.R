library(tidyverse)

setwd("~/GitHub/CareyDataScience/econometrics/project/sandbox")


sales = read_csv("vgsales.csv")


test = lm(EU_Sales ~ JP_Sales, data = sales )
