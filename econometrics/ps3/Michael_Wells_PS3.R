##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometric PS3                       #
# Date: 12/17/20                             #
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
#
# Written Section
#

# Part D ----- 
brfss2010$age_squared = brfss2010$age^2 
lm2 = lm(sm ~ age + age_squared + binge + black + fem + marry + ban + cigtax + inc + edu, data = brfss2010)
summary(lm2)

lm2$coefficients["age"] 
lm2$coefficients["age_squared"] 

# Part E ----
brfss2010$edu_1 = ifelse(brfss2010$edu == 1, 1, 0)
brfss2010$edu_2 = ifelse(brfss2010$edu == 2, 1, 0)
brfss2010$edu_3 = ifelse(brfss2010$edu == 3, 1, 0)
brfss2010$edu_4 = ifelse(brfss2010$edu == 4, 1, 0)


lm3 = lm(sm ~ age + age_squared + binge + black + fem + marry + ban + cigtax + inc + edu_2  + edu_3 + edu_4, data = brfss2010)
summary(lm3)




#---- Problem 2 ----
brfss <- read_csv("brfss.csv")

prev = c(mean(brfss$sm[brfss$year == 2000]), mean(brfss$sm[brfss$year == 2001]), mean(brfss$sm[brfss$year == 2002]), mean(brfss$sm[brfss$year == 2003]), mean(brfss$sm[brfss$year == 2004]), mean(brfss$sm[brfss$year == 2005]))
plot_df = data.frame(year = unique(brfss$year), prev = prev)

ggplot(data = plot_df, aes(x = year, y = prev))+
  geom_point()+
  geom_smooth(method = "lm",formula = y ~ x)


ggplot(brfss, 
       aes(x = year, 
           fill = as.factor(sm))) + 
  geom_bar(position = "fill") +
  facet_wrap(~fips)+
  labs(y = "Proportion")



# Part C ----- 
brfss$post = as.numeric(substr(x = as.character(brfss$year),start = 4, stop = 4))

brfss$treat = ifelse(brfss$fips==34,1,0)
brfss$posttreat = brfss$post * brfss$treat

lm4 = lm(sm ~ post+treat+posttreat,data = brfss)
summary(lm4)

# Part D -----
brfss$post_0 = ifelse(brfss$post == 0, 1, 0)
brfss$post_1 = ifelse(brfss$post == 1, 1, 0)
brfss$post_2 = ifelse(brfss$post == 2, 1, 0)
brfss$post_3 = ifelse(brfss$post == 3, 1, 0)
brfss$post_4 = ifelse(brfss$post == 4, 1, 0)
brfss$post_5 = ifelse(brfss$post == 5, 1, 0)


brfss$posttreat_0 = brfss$post_0 * brfss$treat
brfss$posttreat_1 = brfss$post_1 * brfss$treat
brfss$posttreat_2 = brfss$post_2 * brfss$treat
brfss$posttreat_3 = brfss$post_3 * brfss$treat
brfss$posttreat_4 = brfss$post_4 * brfss$treat
brfss$posttreat_5 = brfss$post_5 * brfss$treat


lm5 = lm(sm ~ treat + post_1 + post_2 + post_3 + post_4 + post_5 + posttreat_1 + posttreat_2 + posttreat_3 + posttreat_4 + posttreat_5,data = brfss)
summary(lm5)










