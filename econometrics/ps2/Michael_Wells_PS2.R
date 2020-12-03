##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometric PS2                       #
# Date: 12/03/20                             #
##############################################

#---- Setup ----
setwd("GitHub/CareyDataScience/econometrics/ps2/")
library(tidyverse)





#---- Problem 1 ----- 
airbnb <- read_csv("airbnb.csv")

# Part a - Log transform 

# Shows the data does not follow normal ditribution 
hist(airbnb$price,breaks = 100)

# Transform shows the new distribtion 
hist(log(airbnb$price))

airbnb$price_log = log(airbnb$price)


# Part b - Build OLS model of price as a function of neighborhood, room type, and availability
airbnb_model = lm(price_log ~  neighbourhood + room_type + availability_365, data = airbnb)

summary(airbnb_model)







#---- Problem 2 -----
foodenjoyment <- read_csv("foodenjoyment.csv")


# Part a - Linear model of enjoyment as a function of food and condiment 

food_model = lm(Enjoyment ~ Food + Condiment + Food * Condiment, data = foodenjoyment)
summary(food_model)




# Part b - Boxplot 
foodenjoyment$class = paste0(foodenjoyment$Food," with ",foodenjoyment$Condiment)
ggplot(foodenjoyment, aes(x=class, y=Enjoyment, fill = class)) + 
  geom_boxplot()+
  geom_jitter()+
  scale_fill_brewer(palette="Blues") + theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())







# ---- Problem 3 ---- 
interaction <- read_csv("interaction.csv")


# Model for 
interaction_model = lm(y ~ x1 + x2 + x1 * x2,data = interaction)
summary(interaction_model)

interaction_model_co = as.list(coefficients(interaction_model))


# Coefficients for X1 model
intercept_x1 = interaction_model_co$`(Intercept)`
slope_x1 = interaction_model_co$x1

# Coefficients for X2 model
intercept_x2 = interaction_model_co$`(Intercept)`+ interaction_model_co$x2
slope_x2 = interaction_model_co$x1 + interaction_model_co$`x1:x2`


# Scatter Plot of interaction data 
ggplot(interaction, aes(x = x1, y = y, col = as.factor(x2)))+
  geom_point()+
  scale_color_brewer(palette="Dark2")+
  geom_abline(intercept = intercept_x1, slope = slope_x1 ) +
  geom_abline(intercept = intercept_x2, slope = slope_x2 )+
  labs(col = "x2")
  
# Box plot of interaction data 
ggplot(interaction, aes(x = x2, y = y, fill = as.factor(x2)))+
  scale_fill_brewer(palette="Dark2") + theme_minimal()+
  geom_jitter()+
  geom_boxplot()

