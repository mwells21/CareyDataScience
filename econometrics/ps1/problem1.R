##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometrics PS1                      #
#                                            #
##############################################



library(tidyverse)
library(pastecs)
library(ggmap)

setwd("~/GitHub/CareyDataScience/econometrics/ps1")
# ---- Problem 1 ---- 

# Part A 
hsb2 <- read_csv("hsb2.csv")
scores = hsb2[,c("read","write","math","science","socst")]
meanscore = rowMeans(scores)
hsb2$meanscore = meanscore

# Part B
hsb2$meancat = NA 
hsb2$meancat[hsb2$meanscore <= 45] = "Low"
hsb2$meancat[hsb2$meanscore > 45 & hsb2$meanscore < 60] = "Middle"
hsb2$meancat[hsb2$meanscore >= 60] = "High"

# Part C 
newdata = hsb2[order(hsb2$meancat),]

# Part D 
newdata2 = newdata[c("read","write","math","science","socst","meanscore","meancat")]



rm(list = ls())
#---- Problem 2 ----
airbnb <- read_csv("airbnb.csv")

# Part A 
mean(airbnb$price)
median(airbnb$price)
max(airbnb$price)
min(airbnb$price)

# Part B
hist(airbnb$price,breaks = 100, col ="powder blue")

# Part C
hist(airbnb$price[airbnb$price < 1000],breaks = 20, col ="powder blue")

# Part D 
plot(airbnb$longitude,airbnb$latitude)

ggplot(data = airbnb, aes(x = longitude, y = latitude,col = airbnb$neighbourhood))+
  geom_point()

# Part E 


# Bonus 
qmplot(longitude, latitude, data = airbnb, maptype = "toner-light", color = room_type)





