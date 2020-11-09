#############################################################################
# Data Science: Econometrics for Market Analysis, Module 2 R Tutorial       #
# Author: Michael Darden                                                    #
# Date: November 1st, 2020                                                  #
# Notes: The # symbol indicates a comment line that R can ignore.           #
#   Commenting code is vital to both accurate data work and to              #
#   replicability                                                           #
#############################################################################
#############################################################################
# The rm() command clears all data/variables from memory.                   #
#############################################################################
rm(list = ls())
#############################################################################
# R is based on a huge number of packages. Some are standard come with the  #
#   the current R distribution.  However, many packages must be loaded. It  #
#   is standard to include the packages you need at the beginning of your   #
#   R script. For today, we will load the following packages:               #
#     1. Tidyverse: Include a large suite of data science packages, tools,  #
#         etc. that are commonly used.  Includes ggplot and dplyr.          #
#     2. readr: helps us read csv data files.                               #
#     3. pastecs: good for summarizing data.                                #
#   The first time your want to use a package, you need to install it.      #
#     To do so, type install.package(). For example, install.package(readr) #
#   Sometimes packages need an update. Type update.packages().              #
#############################################################################
library(tidyverse)
library(readr)
library(pastecs)
#############################################################################
# The setwd command tells R the location on your computer to read and write #
# files. The file name below is specific to my computer. Create a folder on #
# your computer for our class and reference it at the beginning of your R   #
# scripts.                                                                  #
#############################################################################
setwd("~/GitHub/CareyDataScience/econometrics")
#############################################################################
# The read_csv command reads csv files into R.  Note that you must name your#
# data. Here, I have read module1.csv and I name it "dat".  Notice that     #
# after this command, "dat" will appear in the upper right window.          #
#############################################################################
dat = read_csv("module1.csv")
#############################################################################
# Here are some basic commands to better understand your data.              #
#############################################################################
dim(dat)
nrow(dat)
head(dat)
#############################################################################
# Summary Statistics.  Notice that if you are running your entire script,   #
# R will not output your results in the bottom left corner. Adding "print"  #
# will output the results. Notice that you must always tell R both the data #
# and the variable name.                                                    #
#############################################################################
mean(dat$salary)
print(mean(dat$salary))
median(dat$salary)
var(dat$salary)
sd(dat$salary)
min(dat$salary)
max(dat$dcbt)
table(dat$female)
table(dat$female)/nrow(dat)
unique(dat$female)
#############################################################################
# Create new variables and modify existing variables                        #
#############################################################################
dat$newvar = 10
dat$lnsalary = log(dat$salary)
dat$highsal=0
dat$highsal[dat$salary>10]=1
dat = dat[order(-dat$salary),]
justsal = dat[,c(1,5)]


#############################################################################
# Use stat.desc to summarize all data.
#############################################################################
summary(dat$salary)
options(scipen=100)
options(digits=2)
stat.desc(dat)
#############################################################################
# Some Graphics. Notice that these will appear in the plots window.         #
#############################################################################
hist(dat$salary)
hist(dat$salary, xlab="Salary in $10,000 USD", main="Histogram", col="red", border="blue")
qqnorm(dat$salary,main="Retail Firm Salary Data",ylab="Salary Quantiles",xlab="Normal Quantiles")

#############################################################################
# Explore the potential for selection bias by calculating grouped means.    #
#############################################################################
x=aggregate(dat, by=list(dat$dcbt), FUN=mean)
print(x)



#############################################################################
# Linear Regression                                                         #
#############################################################################
regs1 = lm(claims ~ dcbt, data=dat)
print(summary(regs1))
print(coefficients(regs1))
regs2 = lm(claims ~ dcbt + age + female + salary + exp + score +hq, data=dat)
print(summary(regs2))
print(coefficients(regs2))
