setwd("~/GitHub/CareyDataScience/econometrics/lecture7")

# read data ----
dat <- read_csv("Module7.csv")

# Create Binary Variables 
dat$dtype_all = ifelse(dat$dtype == "all",1,0)
dat$dtype_mva = ifelse(dat$dtype == "MVA",1,0)
dat$dtype_suc = ifelse(dat$dtype == "suicide",1,0)
dat$dtype_int = ifelse(dat$dtype == "internal",1,0)

# Without State & Year Specific linear trends 
lm_all = lm(mrate ~ legal + dtype_all ,data = dat)
summary(lm_all)

lm_mva = lm(mrate ~ legal + dtype_mva ,data = dat)
summary(lm_mva)

lm_suc = lm(mrate ~ legal + dtype_suc ,data = dat)
summary(lm_suc)

lm_int = lm(mrate ~ legal + dtype_int ,data = dat)
summary(lm_int)


# Without State Specific linear trends 
lm_all_year = lm(mrate ~ legal + dtype_all + factor(year) ,data = dat)
summary(lm_all_year)

lm_mva_year = lm(mrate ~ legal + dtype_mva + factor(year),data = dat)
summary(lm_mva_year)

lm_suc_year = lm(mrate ~ legal + dtype_suc + factor(year),data = dat)
summary(lm_suc_year)

lm_int_year = lm(mrate ~ legal + dtype_int + factor(year),data = dat)
summary(lm_int_year)



# With State Specific linear trends 
lm_all_state = lm(mrate ~ legal + dtype_all + factor(year) + factor(state),data = dat)
summary(lm_all_state)

lm_mva_state = lm(mrate ~ legal + dtype_mva + factor(year) + factor(state) ,data = dat)
summary(lm_mva_state)

lm_suc_state = lm(mrate ~ legal + dtype_suc + factor(year) + factor(state),data = dat)
summary(lm_suc_state)

lm_int_state = lm(mrate ~ legal + dtype_int + factor(year) + factor(state),data = dat)
summary(lm_int_state)




