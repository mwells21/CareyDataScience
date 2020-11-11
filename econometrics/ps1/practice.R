library(tidyverse)
library(pastecs)
library(ggmap)

# Set Wd
setwd("~/GitHub/CareyDataScience/econometrics/ps1")


# Read Data 
hsb2 <- read_csv("hsb2.csv")

# ---- Problem 1 -----

# Part A 
sub = hsb2[,c(7:11)]
meanScore = rowMeans(sub)

# Par B


# ---- Problem 2  ---- 
airbnb <- read_csv("airbnb.csv")


# Part A 

