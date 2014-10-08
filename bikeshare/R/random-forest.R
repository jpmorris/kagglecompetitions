setwd("/home/jmorris/workspace/kagglecompetitions/bikeshare/R")


train <- read.csv('train.csv')
test <-read.csv('test.csv')

#install.packages("lubridate")
#install.packages("randomForest")
library(lubridate)
library(randomForest)

train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)

train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

test$count<-0

ntry = 9
obb.err = double(ntry)
test.err = double(ntry)

for(mtry in 1:ntry){
        fit <- randomForest(count ~ season + holiday + weather + dow + hour + temp + atemp + humidity + windspeed, data=train, ntree = 400, mtry = mtry)
        obb.err[mtry] = fit$mse[400]
        pred = predict(fit,test)
        test.err[mtry] = with(train, mean((count-pred)^2))
        cat(mtry, " ")
}
matplot(1:mtry, cbind(test.err, oob.err), pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")




fit <- randomForest(count ~ season + holiday + weather + dow + hour + temp + atemp + humidity + windspeed, data=train, ntree = 700, importance=TRUE)
#fit
plot(fit)

Prediction <- predict(fit, test)
plot(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "random-forest.csv", row.names = FALSE)

