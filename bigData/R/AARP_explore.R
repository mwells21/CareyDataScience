library(tidyverse)

setwd("~/GitHub/CareyDataScience/bigData/")

dat = read_csv("data/AARP Data.csv")

users_table = read_csv("data/AARP_users_table.csv")

# ---- Demographics ----

# Gender 
ggplot(users,aes(x = gender, fill = gender) ) +
  geom_bar()+
  scale_fill_manual(values = c("lightpink","lightblue","grey"))

# age 
hist(dat$age_agg_ind,col = "skyblue")

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


# Interactions 
ggplot(users, aes(x = event, y = interactions , fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")

ggplot(users, aes(x = inter_cluser, y = age , fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")


# Gender 
ggplot(users, aes(x = event, y = gender, fill = event)) +
  geom_boxplot()+ theme_minimal()+scale_fill_brewer(palette = "Blues")


#----- State Analysis 

states_counts = table(users$state,users$event) %>% as.data.frame()
colnames(states_counts) = c("State","Type","Count")
states_counts = states_counts[states_counts$Count > 10,]

ggplot(states_counts, aes(x = reorder(State,Count), y = Count, fill = Type))+
  coord_flip() +
  xlab("")+
  ylab("")+
  geom_bar(stat = "identity")+ theme_minimal()+scale_fill_brewer(palette = "Blues")



states = table(users$state,users$event) %>% as.data.frame.matrix()
states = states[states$Virtual > 10,]
states$State = rownames(states)
states$ratio = ((states$Virtual - states$In_Person) / states$Virtual) *100

ggplot(states, aes(x = reorder(State,ratio), y = ratio))+
  coord_flip() +
  xlab("")+
  ylab("")+
  geom_bar(stat = "identity",fill = "skyblue")+ theme_minimal()+scale_fill_brewer(palette = "Blues")


# Merge with lockdown data 
lockdowns = read_csv(file = "../econometrics/project/state_orders.csv")

states$lock_date  = NA 
states$lock_dur   = NA
states$lock_month = NA 
index = match(states$State,lockdowns$Abv)

states$lock_date  = lockdowns$Start[index]
states$lock_dur   = lockdowns$Duration[index]
states$lock_month = lockdowns$Month[index]



states$lock_dur[is.na(states$lock_dur)] = 0
states$lock_dur_cluster = NA 
states = states[!states$State %in% c("PR","U"),]


# clusters 
states$lock_dur_cluster[states$lock_dur <= 50] = "Short"
states$lock_dur_cluster[states$lock_dur > 50] = "Med"
states$lock_dur_cluster[states$lock_dur >= 180] = "Long"



# plots 
ggplot(states, aes(x = reorder(State,ratio), y = ratio,fill = lock_dur_cluster))+
  coord_flip() +
  xlab("In Person / Virtual")+
  ylab("")+
  geom_bar(stat = "identity")+ theme_minimal()+scale_fill_brewer(palette = "Reds",direction = -1)


ggplot(states, aes(x = Virtual ,y = In_Person, col = lock_dur_cluster))+
  coord_flip() +
  geom_text(aes(label=State),hjust=0, vjust=0)+
  geom_point()+theme_minimal()+scale_color_brewer(palette = "Reds",direction = -1)


ggplot(states, aes(x = ratio ,y = lock_dur, col = lock_dur_cluster))+
  coord_flip() +
  geom_text(aes(label=State),hjust=0, vjust=0)+
  geom_point()+theme_minimal()+scale_color_brewer(palette = "Reds",direction = -1)

ggplot(states, aes(x = lock_dur_cluster ,y = ratio, fill = lock_dur_cluster))+
  ylab("% change")+
  geom_boxplot()+theme_minimal()+scale_fill_brewer(palette = "Reds",direction = -1)

t.test(states$ratio[states$lock_dur_cluster == "Short"],states$ratio[states$lock_dur_cluster == "Med"])

# LM model 
lm.states = lm(Virtual ~ In_Person + lock_dur + lock_month  ,data = states)
summary(lm.states)









# ---- PCA -----
library(tidyverse)
library(ggthemes)


# Set dir and load data 
setwd("GitHub/CareyDataScience/bigData/")
dat = read_csv("data/AARP Data.csv")
users = read_csv("data/AARP_users_table.csv")

lockdowns = read_csv(file = "../econometrics/project/state_orders.csv")

users$lock_dur = NA 
users$lock_dur = lockdowns$Duration[match(users$state,lockdowns$Abv)]

pca.users = na.omit(users)
pca.users=pca.users[pca.users$gender!="U",]
pca.users$gender[pca.users$gender == "M"] = 0
pca.users$gender[pca.users$gender == "F"] = 1
pca.users$gender = as.numeric(pca.users$gender)
pca.users$date_joined = NULL
pca.users$state = NULL
pca.users$num_of_events = NULL



index = pca.users[,1:2]
pca.users$id = NULL
pca.users$event = NULL



pca = prcomp(pca.users,center = T,scale. = T)

ggpca = data.frame(id = index,pc1 = pca$x[,1], pc2 = pca$x[,2], pc3 = pca$x[,3])

ggpca = cbind(index, ggpca)


ggplot(ggpca, aes(pc3,pc1, col = id.event)) +
  geom_point()

fig <- plot_ly(ggpca, x = ~pc1, y = ~pc2, z = ~pc3, color = ~id.event, colors = c('blue', 'red','green'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig

