library(tidyverse)
library(caret)
library(vcd)
library(Boruta)
library(randomForest)

# ---- Setup ----
setwd("~/GitHub/CareyDataScience/bigData/")
dat = read_csv("data/AARP_users_table_full.csv")
users = read_csv("data/AARP_users_table.csv")
all = read_csv("data/AARP Data.csv")


# ANOVA 

res.aov = aov(age ~ event, data = users)
summary(res.aov)
TukeyHSD(res.aov)


# CHI - squared Test 

test <- chisq.test(table(users$event,users$gender))
library(vcd)
mosaic(~ event + gender,
       direction = c("v", "h"),
       data = users,
       shade = TRUE)


# Boruta Both Vs In-Person Workflow 
dat[dat == ""] <- NA

# Remove cols with high number of NA's
toRemove = c()
for(i in 1:ncol(dat)){
  bool = sum(!complete.cases(dat[,i])) > 100000
  if(bool == T){
    toRemove = toRemove = c(toRemove,i)
  }
}
dat = dat[,-toRemove]

# Keep complete cases 
dat <- dat[complete.cases(dat),]

# Turn Categorical cols to factor
cat.names = dat %>% select_if(is.character) %>% colnames()

dat[,cat.names] = data.frame(lapply(dat[cat.names], as.factor))
str(dat[,cat.names])

# Seperate X and Y
Y = dat$event
X = dat
X$event = NULL


# Train 
set.seed(123)
boruta.train <- Boruta(x = X, y = Y, doTrace = 2)
print(boruta.train)

save(boruta.train,file = "data/boruta_model_bothVInP.rda")


par(mar=c(10,4,2,1))
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7,)



# PCA 
# Number of Visits 
#dat.filterd = dat[,c("event",getSelectedAttributes(boruta.train, withTentative = F))]
dat.filterd = dat[,c("id","event",names(Labels[(length(Labels) - 15):length(Labels)]))]
index = dat.filterd[,c("id","event")]
dat.filterd$id = NULL
dat.filterd$event = NULL
dat.filterd$BM_activity_dt = NULL
dat.filterd$kx_create_dt = NULL

fac.names = dat.filterd %>% select_if(is.factor) %>% colnames()

dat.filterd[,fac.names] = data.frame(lapply(dat.filterd[fac.names], as.numeric))


pca = prcomp(dat.filterd,center = T,scale. = T)

ggpca = data.frame(id = index$id, event = index$event,pc1 = pca$x[,1], pc2 = pca$x[,2], pc3 = pca$x[,3])




ggplot(ggpca, aes(pc2,pc1, col = event)) +
  geom_point()


fig <- plot_ly(ggpca, x = ~pc1, y = ~pc2, z = ~pc3, color = ~event, colors = c('blue', 'red','green'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))

fig




# Boruta all types Workflow 
# ---- Setup ----
setwd("~/GitHub/CareyDataScience/bigData/")
dat = read_csv("data/AARP_users_table_full.csv")
users = read_csv("data/AARP_users_table.csv")

dat[dat == ""] <- NA

# Remove cols with high number of NA's
toRemove = c()
for(i in 1:ncol(dat)){
  bool = sum(!complete.cases(dat[,i])) > 100000
  if(bool == T){
    toRemove = toRemove = c(toRemove,i)
  }
}
dat = dat[,-toRemove]

# Remove columns that would be NA for virtual users 
dat$BM_activity_dt = NULL
dat$BM_d_ah_mh_summary_key = NULL
dat$BM_response_type = NULL

# Keep complete cases 
sub <- dat[complete.cases(dat),]

# Turn Categorical cols to factor
cat.names = sub %>% select_if(is.character) %>% colnames()

sub[,cat.names] = data.frame(lapply(sub[cat.names], as.factor))
str(sub[,cat.names])

# Seperate X and Y
Y = sub$event
X = sub
X$event = NULL


# Train 
set.seed(123)
boruta.train <- Boruta(x = X, y = Y, doTrace = 2)
print(boruta.train)

save(boruta.train,file = "data/boruta_model_All.rda")

load("data/boruta_model_bothVInP.rda")
par(mar=c(10,4,2,1))
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7,)




ggplot(data = dat, aes(y = 100 - WORK_JOBS_EM,x = event, fill = event))+
  geom_boxplot()+theme_minimal()+scale_fill_brewer(palette = "Blues",direction = -1)+
  ylab("Probability to attend a job working webinar")

ggplot(data = dat, aes(y = NUM_ENTERTAINMENT_VISITS_PAST_3MONTHS,x = event, fill = event))+
  geom_bar(stat = "summary", fun.y = "mean")+theme_minimal()+scale_fill_brewer(palette = "Blues",direction = -1)+
  ylab("Number of Visits")
ggplot(data = dat, aes(y = NUM_ENTERTAINMENT_VISITS_PAST_3MONTHS,x = event, fill = event))+
  geom_bar(stat = "summary", fun.y = "mean")+theme_minimal()+scale_fill_brewer(palette = "Blues",direction = -1)+
  ylab("Number of Visits")
ggplot(data = dat, aes(y = NUM_VISITS_PAST_3MONTHS,x = event, fill = event))+
  geom_bar(stat = "summary", fun.y = "mean")+theme_minimal()+scale_fill_brewer(palette = "Blues",direction = -1)+
  ylab("Number of Visits")
ggplot(data = dat, aes(y = live_answer_am,x = event, fill = event))+
  geom_bar(stat = "summary", fun.y = "mean")+theme_minimal()+scale_fill_brewer(palette = "Blues",direction = -1)+
  ylab("Number of Visits")

ggplot(data = dat, aes(y = NUM_VISITS_PAST_3MONTHS, x = Email_click, col = event))+
  geom_point()

ggplot(data = dat, aes(y = live_answer_am, x = Email_click, col = event))+
  geom_point()
# ---- Predictive Model ----- 
setwd("~/GitHub/CareyDataScience/bigData/")
dat = read_csv("data/AARP_users_table_full.csv")

dat[dat == ""] <- NA

# Remove cols with high number of NA's
toRemove = c()
for(i in 1:ncol(dat)){
  bool = sum(!complete.cases(dat[,i])) > 100000
  if(bool == T){
    toRemove = toRemove = c(toRemove,i)
  }
}
dat = dat[,-toRemove]

# Remove columns that would be NA for virtual users 
dat$BM_activity_dt = NULL
dat$BM_d_ah_mh_summary_key = NULL
dat$BM_response_type = NULL

# Keep complete cases 
dat <- dat[complete.cases(dat),]

# Turn Categorical cols to factor
cat.names = dat %>% select_if(is.character) %>% colnames()

dat[,cat.names] = data.frame(lapply(dat[cat.names], as.factor))
str(dat[,cat.names])

load("data/boruta_model.rda")

dat = dat[,c("event",Boruta::getSelectedAttributes(boruta.train)[Boruta::getSelectedAttributes(boruta.train) %in% colnames(dat)])]

dat = dat[,c("event",names(Labels[41:61])[names(Labels[41:61]) %in% colnames(dat)])]

trainIndex = createDataPartition(dat$event, p = .7, list = F, times = 1)

train <- dat[trainIndex, ]
test <- dat[-trainIndex, ]

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

set.seed(210)
mtryVals <- floor(c(seq(100, 2000, by=100),
                    sqrt(ncol(train))))
mtryGrid <- data.frame(.mtry=mtryVals)

set.seed(420)

RF.obj <- train(event ~ .,
                data = train, 
                method = "rf",
                trControl = fitControl, 
                ntree = 500, 
                importance = TRUE,
                tuneGrid = mtryGrid)


