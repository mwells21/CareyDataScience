library(tidyverse)
library(lubridate)

setwd("GitHub/CareyDataScience/bigData/")

# Full AARP data set
dat = read_csv("data/AARP Data.csv")


# ---- Subset: Virtual and In-Person ----
users = unique(dat$Identification_Key)
users_key = dat[,1:3]

vir = users_key[users_key$TYPE2 == "VIRTUAL",]
per = users_key[users_key$TYPE == "IN_PERSON",]

vir_users = unique(vir$Identification_Key)
per_users = unique(per$Identification_Key)

both_users = vir_users[vir_users %in% per_users]

vir_users = vir_users[!(vir_users %in% both_users)]
per_users = per_users[!(per_users %in% both_users)]


# ---- Create New User based Tidy data frame ----

users_table = data.frame(id = users, event = NA,num_of_events = NA, age = NA, gender = NA,
                         date_joined = NA, days_member = NA,years_member = NA, model_pol = NA,
                         )

# Event 
users_table$event[users_table$id %in% vir_users] = "Virtual"
users_table$event[users_table$id %in% per_users] = "In_Person"
users_table$event[users_table$id %in% both_users] = "Both"


users_index = match(users_table$id,dat$Identification_Key)

# Num of Events 
for( i in 1:length(users)){
  user = users[i]
  users_table$num_of_events[users_table$id == user] = nrow(dat[dat$Identification_Key == user,])
}


# Age
users_table$age = dat$age_agg_ind[users_index]

# Gender 
users_table$gender = dat$gender_agg_ind[users_index]

# Date Joined 
users_table$date_joined = dat$kx_create_dt[users_index]

# Days Member 
users_table$days_member =  Sys.Date() - ymd(users_table$date_joined)

# Years Member 
users_table$years_member =  users_table$days_member / 365

# Politics Model 
users_table$model_pol = dat$AA_model_Ideology[users_index]


# Location - State 
users_table$state 


write_csv(users_table, path = "data/AARP_users_table.csv")


#---- Additions 

users_index = match(users_table$id,dat$Identification_Key)
# Location - State 
users_table$state = NA 
users_table$state = dat$STATE[users_index]


#  ---- Add engagement -----
users_index = match(users$id,dat$Identification_Key)


interactions = dat[users_index,58:72]














# ----- Make whole file user based -----

library(tidyverse)
library(lubridate)

setwd("GitHub/CareyDataScience/bigData/")

# Full AARP data set
dat = read_csv("data/AARP Data.csv")


# ---- Subset: Virtual and In-Person ----
users = unique(dat$Identification_Key)
users_key = dat[,1:3]

vir = users_key[users_key$TYPE2 == "VIRTUAL",]
per = users_key[users_key$TYPE == "IN_PERSON",]

vir_users = unique(vir$Identification_Key)
per_users = unique(per$Identification_Key)

both_users = vir_users[vir_users %in% per_users]

vir_users = vir_users[!(vir_users %in% both_users)]
per_users = per_users[!(per_users %in% both_users)]


users_table = data.frame(id = users, event = NA)


# Event 
users_table$event[users_table$id %in% vir_users] = "Virtual"
users_table$event[users_table$id %in% per_users] = "In_Person"
users_table$event[users_table$id %in% both_users] = "Both"


users_index = match(users_table$id,dat$Identification_Key)


for(i in 4:ncol(dat)){
  tmpCol = dat[users_index,i]
  users_table = cbind(users_table,tmpCol)
}

write_csv(users_table, path = "data/AARP_users_table_full.csv")

