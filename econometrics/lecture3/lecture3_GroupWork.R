library(tidyverse)
library(pastecs)
setwd("~/GitHub/CareyDataScience/econometrics/lecture3")

# ---- Group Work ---- 
auto <- read_csv("auto.csv")


# Data Stats 
stat.desc(auto)

# Mean for all columns between foreign and non-foreign 
aggregate(auto, by=list(auto$foreign), FUN=mean)

# Regress Price for mpg
auto_regs = lm(price ~ mpg,data = auto)
summary(auto_regs)

# Regress Price for mpg, foreign, and weight 
auto_regs2 = lm(price ~ mpg + foreign + weight ,data = auto)
summary(auto_regs2)

# Simple Plot mpg vs price 
ggplot(data = auto, aes(x = mpg, y = price, col = as.character(foreign)))+
  geom_point()


