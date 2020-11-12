##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometric PS1                       #
#                                            #
##############################################


library(tidyverse)
library(ggmap)

setwd("~/GitHub/CareyDataScience/econometrics/ps1")
# ---- Problem 1 ---- 
hsb2 <- read_csv("hsb2.csv")

# Part A 
scores = hsb2[,c("read","write","math","science","socst")]
meanscore = rowMeans(scores)
hsb2$meanscore = meanscore

# Part B
hsb2$meancat = NA 
hsb2$meancat[hsb2$meanscore <= 45] = "Low"
hsb2$meancat[hsb2$meanscore > 45 & hsb2$meanscore < 60] = "Middle"
hsb2$meancat[hsb2$meanscore >= 60] = "High"

# Part C 
newdata = hsb2[order(hsb2$meanscore),]

# Part D 
newdata2 = newdata[c("read","write","math","science","socst","meanscore","meancat")]



rm(list = ls())
#---- Problem 2 ----
airbnb <- read_csv("airbnb.csv")

# Part A - General Stats
mean(airbnb$price)     # 198.0681
median(airbnb$price)   # 135
max(airbnb$price)      # 10000
min(airbnb$price)      # 10

# Part B - Histogram
hist(airbnb$price,breaks = 100,main = "AirBnB Price", xlab = "Price", col ="powder blue")

# Part C - Filtered Histogram
hist(airbnb$price[airbnb$price < 1000],breaks = 20,main = "AirBnB Price", xlab = "Price", col ="powder blue")

# Part D - Long vs Lat Scatter Plot
plot(airbnb$longitude,airbnb$latitude, main = "AirBnB Location",xlab = "Longitude", ylab = "Latitude")

# Colored by roomtype 
ggplot(data = airbnb, aes(x = longitude, y = latitude,col = airbnb$room_type))+
  geom_point()

# Part E - Removed location that is in the river 
airbnb_clean = airbnb[airbnb$latitude >= 29.91,]
plot(x = airbnb_clean$longitude,y = airbnb_clean$latitude,main = "AirBnB Location",xlab = "Longitude", ylab = "Latitude")


# Bonus - color by room type, size by price
qmplot(longitude, latitude, data = airbnb_clean, maptype = "toner-lite", color = room_type, size = price)

# Facet
qmplot(longitude, latitude, data = airbnb_clean, maptype = "toner-lite", color = room_type, size = price)+
  facet_wrap(~room_type)



# EXTRA - Because I was curious 
p <- ggplot(airbnb[airbnb$price <= 1000,], aes(x=room_type, y=price, fill = room_type)) + 
  geom_boxplot()
p

