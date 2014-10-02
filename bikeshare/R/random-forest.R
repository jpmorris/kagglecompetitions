setwd("/home/jmorris/workspace/kagglecompetitions/bikeshare")


train <- read.csv('train.csv')
test <-read.csv('test.csv')

install.packages("lubridate")
install.packages("randomForest")
library(lubridate)
library(randomForest)

train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)

train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)

test$count<-0

fit <- randomForest(as.factor(count) ~ season + holiday + weather + dow + hour + temp + atemp + humidity + windspeed, data=train, ntree = 700, importance=TRUE)
plot(fit)

Prediction <- predict(fit, test)
plot(Prediction)
submit <- data.frame(datetime = test$datetime, count = Prediction)
write.csv(submit, file = "random-forest.csv", row.names = FALSE)

