setwd("~/GitHub/CareyDataScience/biostats/week1")

library(tidyverse) 


# ---- Problem 1 ---- 
ps1 = read_csv("baltps11.csv")

# Create new variables for stem and leaf plots 
ps1.1 = filter(ps1, group==1)
ps1.2 = filter(ps1, group==2)

# Stem and Leaf Plots 
stem(ps1.1$deaths)
stem(ps1.2$deaths) 

stem(ps1.1$deaths, scale=1)
stem(ps1.1$deaths, scale=3)

stem(ps1.2$deaths, scale=1) 
stem(ps1.2$deaths, scale=3) 

# Boxplot for stem and leaf data
boxplot(deaths ~ group, data=ps1) 


# ---- Probelem 2 ----- 
ce621 <- read_csv("ce621.csv") 

ce621.male = filter(ce621, sex=="Male")
ce621.female = filter(ce621, sex=="Female")
stem(ce621.male$totchg)
stem(ce621.female$totchg) 

# Sumarize the data 
summary(ce621.male$totchg)
sd(ce621.male$totchg)
mean(ce621.male$totchg)
x = c(0,.1,.23,.9,1)
quantile(ce621.male$totchg,probs = x)


# Sumarize the data 
summary(ce621.female$totchg)
sd(ce621.female$totchg)
quantile(ce621.female$totchg, c(0,.1,.25,.5,.75,.9,1))


# Box plot for 
ce621e <- read_csv("ce621entire.csv")
ce621e95 = filter(ce621e, year==1995)
ce621e95$agecat = cut(ce621e95$age, c(0, 50, 64, 100),
                      right=TRUE, labels=c("<=50","51-64",">=65"))
tmp  = ce621e95[ce621e95$totchg < 20000,]
boxplot(totchg ~ sex + agecat, data=ce621e95, names=c("F <= 50","M <= 50","F 51-64", "M 51-64", "F >=65", "M >=65")) 

ce621e95$log10chg = log10(ce621e95$totchg)
boxplot(log10chg ~ sex + agecat, data=ce621e95,
        names=c("F <= 50", "M <= 50","F 51-64", "M 51-64","F >=65", "M >=65"))












