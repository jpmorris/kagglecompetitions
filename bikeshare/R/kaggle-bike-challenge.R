#--------------------------------------------------------
#  Conditional Inference Tree for kaggle-bike-sharing
#  Brandon Harris
#  www.brandonharris.io
#  @nameBrandon
#--------------------------------------------------------

#setwd("/users/Brandon/Dropbox/Kaggle/Bike Sharing/")
#read in train/test

train <- read.csv("train.csv")
test <- read.csv("test.csv")

#str(train)

#factorize training set
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(train$season)

#factorize test set
test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(test$season)

#create time column by stripping out timestamp
train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)

#factorize new timestamp column
train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)

#create day of week column
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

aggregate(train_factor[,"count"],list(train_factor$day),mean)

#create Sunday variable
train_factor$sunday[train_factor$day == "Sunday"] <- 1
train_factor$sunday[train_factor$day != "Sunday"] <- 0

test_factor$sunday[test_factor$day == "Sunday"] <- 1
test_factor$sunday[test_factor$day != "Sunday"] <- 0


train_factor$saturday[train_factor$day == "Saturday"] <- 1
train_factor$saturday[train_factor$day != "Saturday"] <- 0

test_factor$saturday[test_factor$day == "Saturday"] <- 1
test_factor$saturday[test_factor$day != "Saturday"] <- 0



#convert to factor
#train_factor$sunday <- as.factor(train_factor$sunday)
#test_factor$sunday <- as.factor(test_factor$sunday)

#convert time and create $hour as integer to evaluate
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))


#4AM - 9AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$daypart[(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1


#10AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$daypart[(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2


#4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$daypart[(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)

#convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)


# new variable work=0:workday, 1:weekend, 2:holiday
train_factor$work=ifelse(train_factor$holiday==0 & train_factor$workingday==1,0,ifelse(train_factor$holiday==1 & train_factor$workingday==0,2,1))
train_factor$work<-as.factor(train_factor$work)

test_factor$work=ifelse(test_factor$holiday==0 & test_factor$workingday==1,0,ifelse(test_factor$holiday==1 & test_factor$workingday==0,2,1))
test_factor$work<-as.factor(test_factor$work)



#install party package
#install.packages('party')
library('party')

#build our formula
formulaCount <- count ~  season + weather + atemp + humidity +hour +daypart +day+work
#formulaCasual <- casual ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday
#formulaRegistered <- registered ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday




#formulaCasual <- casual ~  season + weather + atemp + humidity +hour +daypart +day+work
#formulaRegistered <- registered ~  season + weather + atemp + humidity +hour +daypart +day+work




#build our model
fit.ctree.count <- ctree(formulaCount, data=train_factor)
#fit.ctree.casual <- ctree(formulaCasual, data=train_factor)
#fit.ctree.registered <- ctree(formulaRegistered, data=train_factor)


#examine model for variable importance
#fit.ctree


#run model against test data set
predict.train.ctree.count <- round(predict(fit.ctree.count, train_factor))
#predict.train.ctree.casual <- round(predict(fit.ctree.casual, train_factor))
#predict.train.ctree.registered <- round(predict(fit.ctree.registered, train_factor))

#print(sqrt(sum((log(predict.train.ctree.casual+predict.train.ctree.registered+1)-log(train$count+1))**2)/10000.0))
print(sqrt(sum((log(predict.train.ctree.count+1)-log(train$count+1))**2)/10000.0))



#run model against test data set
#predict.ctree.casual <- predict(fit.ctree.casual, test_factor)
#predict.ctree.registered <- predict(fit.ctree.registered, test_factor)
#predict.ctree <- predict.ctree.casual+predict.ctree.registered
predict.ctree <- predict(fit.ctree.count, test_factor)

#build a dataframe with our results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)
colnames(submit.ctree) <- c("datetime","count")
#write results to .csv for submission
write.csv(submit.ctree, file="submit_ctree_v1.csv",row.names=FALSE)


