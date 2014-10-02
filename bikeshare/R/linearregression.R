setwd("/home/jmorris/workspace/kagglecompetitions/bikeshare")

# quick and dirty test of submission
train <- read.csv('train.csv')
test <-read.csv('test.csv')
str(train)

library(lubridate)

train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)

train$dow <- wday(train$datetime)
test$dow <- wday(test$datetime)



linearmodel <- lm(count ~ season + holiday + weather + dow + hour + temp + atemp + humidity + windspeed, data=train)
summary(linearmodel)

predictTest <- predict(linearmodel, newdata=test)
predictTest[predictTest < 0] <- 0
#predictTest <- abs(predictTest)

submit <- data.frame(datetime = test$datetime, count = predictTest)
write.csv(submit, file="linearregression.csv", row.names = FALSE, quote = FALSE)


