##############################################
# Author: Michael Wells                      #
# At: Johns Hopkins Carey Business School    # 
# For: Econometric PS3                       #
# Date: 12/18/20                             #
##############################################

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


# Part E ----
brfss2010$edu_1 = brfss2010$edu
brfss2010$edu_2 = brfss2010$edu
brfss2010$edu_3 = brfss2010$edu
brfss2010$edu_4 = brfss2010$edu

brfss2010$edu_1[brfss2010$edu_1 != 1] = 0
brfss2010$edu_2[brfss2010$edu_2 != 2] = 0
brfss2010$edu_3[brfss2010$edu_3 != 3] = 0
brfss2010$edu_4[brfss2010$edu_4 != 4] = 0

brfss2010$edu_1[brfss2010$edu_1 == 1] = 1
brfss2010$edu_2[brfss2010$edu_2 == 2] = 1
brfss2010$edu_3[brfss2010$edu_3 == 3] = 1
brfss2010$edu_4[brfss2010$edu_4 == 4] = 1

lm3 = lm(sm ~ age + age_squared + binge + black + fem + marry + ban + cigtax + inc + edu_1 + edu_2  + edu_3 + edu_4, data = brfss2010)
summary(lm3)











