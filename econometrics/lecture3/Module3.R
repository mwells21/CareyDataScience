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
setwd("~/GitHub/CareyDataScience/econometrics/lecture3")
#############################################################################
# The read_csv command reads csv files into R.  Note that you must name your#
# data. Here, I have read module1.csv and I name it "dat".  Notice that     #
# after this command, "dat" will appear in the upper right window.          #
#############################################################################
dat = read_csv("module1.csv")
#############################################################################
# Use stat.desc to summarize all data.
#############################################################################
summary(dat$salary)
options(scipen=100)
options(digits=2)
stat.desc(dat)
#############################################################################
# Explore the potential for selection bias by calculating grouped means.    #
#############################################################################
y=aggregate(dat, by=list(dat$dcbt), FUN=mean)
print(y)
#############################################################################
# Linear Regression                                                         #
#############################################################################
regs1 = lm(claims ~ dcbt, data=dat)
print(summary(regs1))
print(coefficients(regs1))
regs2 = lm(claims ~ dcbt + age + female + salary + exp + score +hq, data=dat)
print(summary(regs2))
print(coefficients(regs2))


library(tidyverse)
library(readr)
# ---- Group Work ---- 
auto <- read_csv("auto.csv")

stat.desc(auto)
aggregate(auto, by=list(auto$foreign), FUN=mean)


auto_regs = lm(price ~ mpg,data = auto)
summary(auto_regs)

auto_regs2 = lm(price ~ mpg + foreign + weight ,data = auto)
summary(auto_regs2)


ggplot(data = auto, aes(x = mpg, y = price, col = as.character(foreign)))+
  geom_point()






