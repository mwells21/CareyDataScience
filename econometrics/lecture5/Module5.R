#############################################################################
# Data Science: Econometrics for Market Analysis                            #
# Author: Michael Darden                                                    #
# Date: November 1st, 2020                                                  #
# Notes:                                                                    #
#############################################################################
rm(list=ls())
library(tidyverse)
library(readr)
library(pastecs)
library(rdrobust)

setwd("~/Dropbox/Teaching/Hopkins/Econometrics/R")
dat = read_csv("rdd.csv")

#dat$da=0
#dat$da[dat$x>5] = 1

dat$da = ifelse(dat$x>5,1,0)



dat$run=(dat$x-5)
dat$run2=(dat$x-5)^2
dat$rund=(dat$x-5)*dat$da
dat$run2d=((dat$x-5)^2)*dat$da


rdd_l=lm(y~da+run+rund, data=dat)
summary(rdd_l)

rdd_q=lm(y~da+run+run2+rund+run2d, data=dat)
summary(rdd_q)

#### Graphs

# Scatter plot
ggplot(dat, aes(x=x, y=y)) + geom_point()
#   w/ linear best-fit line
ggplot(dat, aes(x=x, y=y)) + geom_point() + geom_smooth(method=lm, se=FALSE)

rdplot(dat$y,dat$x, c=5, p=5)                                                                                                                                                          
