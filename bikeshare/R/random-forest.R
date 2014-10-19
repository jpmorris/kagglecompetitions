setwd("/home/jmorris/workspace/kagglecompetitions/bikeshare/R")


train <- read.csv('train.csv')
test <-read.csv('test.csv')

#install.packages("lubridate")
#install.packages("randomForest")
library(lubridate)
library(randomForest)
library(party)
library(gbm)
library(ISLR)
library(boot)


# ADDITIONAL FEATURES
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)
 

train$daypart <- 4
test$daypart <- 4
## 3AM - 10AM = 1
train$daypart[(train$hour > 3) & (train$hour < 10)] <- 1
test$daypart[(test$hour > 3) & (test$hour < 10)] <- 1
## 11AM - 3PM = 2
train$daypart[(train$hour > 9) & (train$hour < 16) ] <- 2
test$daypart[(test$hour > 9) & (test$hour < 16) ] <- 2
## 4PM - 9PM = 3
train$daypart[(train$hour > 15) & (train$hour < 22)] <- 3
test$daypart[(test$hour > 15) & (test$hour < 22)] <- 3


#FACTORIZE FEATURES
train$weather <- factor(train$weather)
test$weather <- factor(test$weather)
train$holiday <- factor(train$holiday)
test$holiday <- factor(test$holiday)
train$workingday <- factor(train$workingday)
test$workingday <- factor(test$workingday)
train$season <- factor(train$season)
test$season <- factor(test$season)
#train$hour <- factor(train$hour)
#test$hour <- factor(test$hour)
train$dow <- factor(train$dow)
test$dow <- factor(test$dow)
train$daypart <- factor(train$daypart)
test$daypart <- factor(test$daypart)
test$count<-0


# VALIDATION
# Holdout validation
library(caTools)
split <- sample.split(train$count, SplitRatio = 0.7)
partialtrain <- subset(train, split==TRUE)
holdoutvalidation <- subset(train, split==FALSE)

# Cross validation





# MSE v.s. forest mtry
#ntry = 9
#obb.err = double(ntry)
#test.err = double(ntry)
#for(mtry in 1:ntry){
#        fit <- randomForest(count ~ season + holiday + weather + dow + hour + temp + atemp + humidity + windspeed, data=train, ntree = 400, mtry = mtry)
#        obb.err[mtry] = fit$mse[400]
#        pred = predict(fit,test)
#        test.err[mtry] = with(train, mean((count-pred)^2))
#        cat(mtry, " ")
#}
#matplot(1:mtry, cbind(test.err, oob.err), pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")



#fit
# random forest
#fit <- randomForest(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + dow + daypart, data=train, ntree = 700, importance=TRUE)
# conditional inference trees
fit <- ctree(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + dow + daypart, data=train)
# boosting
fit <- gbm(count ~ season + holiday + workingday + weather + temp + atemp + humidity + windspeed + hour + dow + daypart, data=train, distribution="gaussian", n.trees=10000, shrinkage=0.01, interaction.depth=4)



plot(fit)

Prediction <- predict(fit, test)

# boosting predict - this wont work with a test dataset without count values (need to crossvalidate)
n.trees=seq(from=100, to=10000,by=100)
Prediction <- predict(fit, test, n.trees=n.trees)
dim(Prediction)
berr = with(test, apply((Prediction - count)^2, 2, mean))
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", 
     main = "Boosting Test Error")

plot(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "random-forest.csv", row.names = FALSE)

