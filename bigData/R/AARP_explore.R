library(tidyverse)

setwd("GitHub/CareyDataScience/bigData/")

dat = read_csv("data/AARP Data.csv")

users_table = read_csv("data/AARP_users_table.csv")

# ---- Demographics ----

# Gender 
ggplot(users_table,aes(x = gender, fill = gender) ) +
  geom_bar()+
  scale_fill_manual(values = c("lightpink","lightblue","grey"))

# age 
hist(dat$age_agg_ind)

# Marital Status / Gender
dat$marital_stat_agg_ind = as.factor(dat$marital_stat_agg_ind)
levels(dat$marital_stat_agg_ind) = c("Alternative/Inferred Married","Inferred single","Divorced",
                                     "Inferred married","Married","Separated","Single","Not Known to be Married","widowed")


ggplot(dat, aes(x = marital_stat_agg_ind, fill = gender_agg_ind))+
  geom_bar()+
  scale_fill_manual(values = c("lightpink","lightblue","grey"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Employment Status  
ggplot(dat, aes(x = employment_stat, fill = gender_agg_ind))+
  geom_bar()+
  scale_fill_manual(values = c("lightpink","lightblue","grey"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



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

# ---- PCA -----


# Number of Visits 
nums = dat[,c(1,57:72)] %>% na.omit()
index = nums$Identification_Key
nums$Identification_Key = NULL

pca = prcomp(nums,center = T)

ggpca = data.frame(id = index,pc1 = pca$x[,1], pc2 = pca$x[,2], pc3 = pca$x[,3])

ggpca$type = NA 
ggpca$type[ggpca$id %in% vir_users] = "Virtual"
ggpca$type[ggpca$id %in% per_users] = "In-Person"
ggpca$type[ggpca$id %in% both_users] = "Both"


ggplot(ggpca, aes(pc1,pc3, col = type)) +
  geom_point()




########################################################################
library(tidyverse)
library(ggthemes)


# Set dir and load data 
setwd("GitHub/CareyDataScience/bigData/")
dat = read_csv("data/AARP Data.csv")
users = read_csv("data/AARP_users_table.csv")

# Bar plot counts for each group
ggplot(users, aes(x = event,fill = event)) +
  geom_bar()+ theme_minimal()+ scale_fill_brewer(palette = "Blues")

# Boxplots comparing types 

# Age 
ggplot(users, aes(x = event, y = age, fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")

t.test(users$age[users$event == "In_Person"],users$age[users$event == "Virtual"])

# Politics Model 
ggplot(users, aes(x = event, y = model_pol, fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")
  
t.test(users$age[users$event == "In_Person"],users$ge[users$event == "Virtual"])

# days since joining 
ggplot(users, aes(x = event, y = days_member, fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")

# Number of events 
ggplot(users, aes(x = event, y = num_of_events, fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")


ggplot(users, aes(x = event, y =num_of_events, fill = event)) +
  geom_bar(stat = "summary",fun.y = "mean")+ theme_minimal()+scale_fill_brewer(palette = "Blues")+
  ylab("Average Number of Events")

















