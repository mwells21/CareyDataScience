library(tidyverse)
library(lubridate)


setwd("~/GitHub/CareyDataScience/stats/week1/")

# ---- Load Data ---- 
CEOdata = read_csv("data/CEOdata.csv")
conret  = read_csv("data/conret.csv")



# ---- Problem 1 ----

# 1.1 - Compensation 
summary(CEOdata$Compensation)
sd(CEOdata$Compensation)
var(CEOdata$Compensation)

# 1.1 - Age 
summary(CEOdata$Age)
sd(CEOdata$Age)
var(CEOdata$Age)


# 1.3 - Histograms 
hist(CEOdata$Compensation, col = 'skyblue2', breaks = 10)
hist(CEOdata$Age, col = 'skyblue2', breaks = 10)


# 1.4 - Create new column for the ln(Compensation)
CEOdata$logComp = log(CEOdata$Compensation)
head(CEOdata$logComp)


# 1.5 - Summarize logComp
summary(CEOdata$logComp)
sd(CEOdata$logComp)
var(CEOdata$logComp)

# 1.6 - logComp hist
hist(CEOdata$logComp, col = 'skyblue2')

# 1.7 - Scatter plot logComp vs Age 
ggplot(CEOdata, aes(x=Age, y=logComp)) + 
  geom_point()+
  geom_smooth(method=lm)




# ---- Problem 2 ---- 
conret$year = substr(x= conret$date, start = 1, stop = 4)
conret$month = substr(x= conret$date, start = 5, stop = 6)
conret$day = 01
conret$Date = mdy(paste(conret$month,conret$day, conret$year,sep = "-"))

# 2.1 - Time plot for Canadian Returns
ggplot(conret, aes(x=Date, y=canada)) +
  geom_line( color="steelblue") + 
  geom_point()+
  labs(title = "Time Series Plot for Canadian Returns")

# 2.2 - Time plot for Japanese Returns
ggplot(conret, aes(x=Date, y=japan)) +
  geom_line( color="steelblue") + 
  geom_point()+
  labs(title = "Time Series Plot for Japanese Returns")


# 2.3 - Histograms of Canada and Japan 
hist(conret$canada,col = "skyblue2",breaks = 20,xlim = c(-.2,.3))
hist(conret$japan,col = "skyblue2",breaks = 20,xlim = c(-.2,.3))


# 2.4 - Summary table of data 
countries = colnames(conret)[2:16]

# place holder set equal to null
sumTable = NULL
for(i in 1:length(countries)){
  this.country = countries[i]
  this.data    = as.data.frame(conret[,this.country])
  
  # Calculate needed info
  this.mean    = signif(mean(this.data[,1]),digits = 3)
  this.median  = signif(median(this.data[,1]),digits = 3)
  this.sd      = signif(sd(this.data[,1]),digits = 3)
  this.var     = signif(var(this.data[,1]),digits = 3)
  
  # build data frame 
  this.tmp     = data.frame(country = this.country, mean = this.mean, median = this.median, sd = this.sd, variance = this.var)
  if(is.null(sumTable)){
    sumTable = this.tmp
  } else {
    sumTable = rbind(sumTable, this.tmp)
  }
  
  
  
}

write_csv(sumTable, path = "data/countrySummary.csv")










