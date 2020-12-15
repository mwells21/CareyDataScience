library(tidyverse)

setwd("~/GitHub/CareyDataScience/econometrics/lecture6")

did <- read_csv("did.csv")


# ---- State ---- 
table(did$fips) / nrow(did)
# ---- Year ---- 
table(did$year) / nrow(did)
# ---- Smoking ---- 
table(did$sm) / nrow(did)

# ---- Smoke / State ----- 
table(did$sm, did$fips) / nrow(did)

table(did$sm[did$fips == 34]) / table(did$fips)["34"]
table(did$sm[did$fips == 42]) / table(did$fips)["42"]

# ---- Smoke / Year ----- 
table(did$sm[did$year == 2004]) / table(did$year)["2004"]
table(did$sm[did$year == 2008]) / table(did$year)["2008"]

# ---- Model ----
lm(data = did, )