### These are sample codes for the examples used in our last Statistics Class (linear regressions)
### Instructor: Yiqing Xing
### This version: 10/08/2020
### Life exp vs Health expenditure
### 2014 OECD data based on Source: http://www.oecd.org/els/health-systems/OECD-Health-Statistics-2016-Frequently-Requested-Data.xls

## Set working folder (Change to your own folder)
setwd("~/GitHub/CareyDataScience/stats/regression/")


## Read the data, notice that there is a header row
health <- read.csv("Health_LifeExpectency.csv", header=TRUE)

# Have a quick look at your data and summary statistics
View(health)
summary(health)

# In case one interested in transformed data, one can use
health$life_exp_log <- log(health$life_exp)
health$health_expenditure_2 <- health$health_expenditure^2


# drop non-OECD countries (if any)
health <- health[health$OECD == 1,]

## Regressions (You can run many regressions together, and save the results for later use)

lm_life_spending <- lm(life_exp  ~ health_expenditure , data = health)

lm_life_smoker <- lm(life_exp  ~ smoker_perc , data = health)

lm2 <- lm(life_exp  ~ health_expenditure + smoker_perc, data = health)

lm3 <- lm(life_exp  ~ health_expenditure + smoker_perc , data = health)

lm4 <- lm(life_exp  ~ health_expenditure + smoker_perc + alcohol, data = health)

lm5 <- lm(life_exp  ~ health_expenditure + smoker_perc + alcohol + obese_perc, data = health)

lm6 <- lm(life_exp  ~ health_expenditure + health_expenditure_2, data = health) 

# non-USA: add conditions in "data" part

lm_life_spending_nonUS <- lm(life_exp  ~ health_expenditure , data = subset(health, country_code != "USA") )

## Display your regression results

summary(lm_life_spending)

summary(lm2)

summary(lm3)

summary(lm4)

summary(lm5)

summary(lm_life_spending_nonUS)


## Plot your data

plot(life_exp  ~ health_expenditure, data = health)

# To make your plot prettier, there are several parameters that you can put in 

plot(life_exp  ~ health_expenditure, data = health, pch = 16, cex = 1.3, col = "blue", main = "Life Expectancy vs Health Spending (OECD)", xlab = "Health Spending per capita (in 1k US$)", ylab = "Life Expectancy")

# If we want to add country code:
with(health, text(life_exp  ~ health_expenditure, labels = health$country_code))

# It is too hard to see, so let'e redo the above by making the data dots invisible (in white)

plot(life_exp  ~ health_expenditure, data = health, pch = 16, cex = 1.3, col = "white", main = "Life Expectancy vs Health Spending (OECD)", xlab = "Health Spending per capita (in 1k US$)", ylab = "Life Expectancy")

with(health, text(life_exp  ~ health_expenditure, labels = health$country_code))


## Adding fitted line to your plot

abline(lm_life_spending, lwd = 4)

abline(lm_life_spending_nonUS, col = "red", lwd = 2)


## Prediction and confidence intervals

# maybe you want to redo the plot
plot(life_exp  ~ health_expenditure, data = health, pch = 16, cex = 1, col = "blue", main = "Life Expectancy vs Health Spending (OECD)", xlab = "Health Spending per capita (in 1k US$)", ylab = "Life Expectancy")
abline(lm_life_spending, lwd = 4)

# To plot CI curves, first let's sort the data (you can try if you don't do this)

health <- health[order(health$health_expenditure),]

conf_interval <- predict(lm_life_spending, health, interval="confidence",level = 0.90)
lines(health$health_expenditure, conf_interval[,2], col="brown", lty=2, lwd = 2)
lines(health$health_expenditure, conf_interval[,3], col="brown", lty=2, lwd = 2)


conf_interval_99 <- predict(lm_life_spending, health, interval="confidence",level = 0.99)
lines(health$health_expenditure, conf_interval_99[,2], col="purple", lty=1, lwd = 2)
lines(health$health_expenditure, conf_interval_99[,3], col="purple", lty=1, lwd = 2)

pred_interval <- predict(lm_life_spending, health, interval="prediction",level = 0.90)
lines(health$health_expenditure, pred_interval[,2], col="blue", lty=3, lwd = 2)
lines(health$health_expenditure, pred_interval[,3], col="blue", lty=3, lwd = 2)


# Plot life_expectancy on smoking 
plot(life_exp  ~ smoker_perc, data = health, pch = 16, cex = .1, col = "blue", main = "Life Expectancy vs Smoker Percentage (OECD)", xlab = "Smoker percentage", ylab = "Life Expectancy")

with(health, text(life_exp  ~ smoker_perc, labels = health$country_code))

abline(lm9, lwd = 3)

# Additional codes for Week 7

plot(life_exp  ~ health_expenditure, data = health, pch = 16, cex = 1, col = "white", main = "Life Expectancy vs Health Spending (OECD)", xlab = "Health Spending per capita (in 1k US$)", ylab = "Life Expectancy")
with(health, text(life_exp  ~ health_expenditure, labels = health$country_code))

abline(lm_life_spending, lwd = 3)

abline(lm6, lwd = 3, col = "red")

predictedcounts <- predict(lm6,list(health$health_expenditure, health$health_expenditure))


glm(life_exp  ~ health_expenditure + health_expenditure*health_expenditure, data = health) 


## Enjoy the software :)
