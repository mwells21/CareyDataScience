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
dat = read_csv("../lecture6/did.csv")

### Define Variables
dat$post = ifelse(dat$year==2008,1,0)
dat$treat = ifelse(dat$fips==34,1,0)
dat$posttreat = dat$post * dat$treat

#### Summarize Data
mean(dat$sm[which(dat$post==0 & dat$treat==0)])
mean(dat$sm[which(dat$post==1 & dat$treat==0)])
mean(dat$sm[which(dat$post==0 & dat$treat==1)])
mean(dat$sm[which(dat$post==1 & dat$treat==1)])

### Raw DiD
did = (mean(dat$sm[which(dat$post==1 & dat$treat==1)])-mean(dat$sm[which(dat$post==0 & dat$treat==1)])) - (mean(dat$sm[which(dat$post==1 & dat$treat==0)])-mean(dat$sm[which(dat$post==0 & dat$treat==0)]))
print(did)

### Difference-in-differences estimator
did1 = lm(sm~post+treat+posttreat,data=dat)
print(summary(did1))


