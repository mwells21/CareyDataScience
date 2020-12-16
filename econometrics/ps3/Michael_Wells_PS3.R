##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometric PS3                       #
# Date: 12/18/20                             #
##############################################
library(tidyverse)
library(rdrobust)

setwd("~/GitHub/CareyDataScience/econometrics/ps3/")

# ---- Problem 1 ---- 
brfss2010 <- read_csv("brfss2010.csv")


# Part A ---- 
lm1 = lm(sm ~ age + binge + black + fem + marry + ban + cigtax + inc + edu, data = brfss2010)
summary(lm1)

# Part B ---- 
lm1$coefficients["inc"] * 100 

# Part C ----- 


# Part D ----- 
brfss2010$age_squared = brfss2010$age^2 
lm2 = lm(sm ~ age + age_squared + binge + black + fem + marry + ban + cigtax + inc + edu, data = brfss2010)
summary(lm2)

rdplot(brfss2010$sm,dat$age, c=5, p=5) 


# Part E ----
brfss2010$edu_1 = ifelse(brfss2010$edu == 1, 1, 0)
brfss2010$edu_2 = ifelse(brfss2010$edu == 2, 1, 0)
brfss2010$edu_3 = ifelse(brfss2010$edu == 3, 1, 0)
brfss2010$edu_4 = ifelse(brfss2010$edu == 4, 1, 0)


lm3 = lm(sm ~ age + age_squared + binge + black + fem + marry + ban + cigtax + inc + edu_1 + edu_2  + edu_3 + edu_4, data = brfss2010)
summary(lm3)




#---- Problem 2 ----
brfss <- read_csv("brfss.csv")

prev = c(mean(brfss$sm[brfss$year == 2000]), mean(brfss$sm[brfss$year == 2001]), mean(brfss$sm[brfss$year == 2002]), mean(brfss$sm[brfss$year == 2003]), mean(brfss$sm[brfss$year == 2004]), mean(brfss$sm[brfss$year == 2005]))
plot_df = data.frame(year = unique(brfss$year), prev = prev)

ggplot(data = plot_df, aes(x = year, y = prev))+
  geom_point(color = "")+
  geom_smooth(method = "lm",formula = y ~ x)


ggplot(brfss, 
       aes(x = year, 
           fill = as.factor(sm))) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")



# Part C ----- 
brfss$post = as.numeric(substr(x = as.character(brfss$year),start = 4, stop = 4))

brfss$treat = ifelse(brfss$fips==34,1,0)
brfss$posttreat = brfss$post * brfss$treat

lm4 = lm(sm ~ post+treat+posttreat,data = brfss)
summary(lm4)













