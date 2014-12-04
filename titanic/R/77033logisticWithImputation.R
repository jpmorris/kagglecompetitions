# kaggle Titanic 

setwd('/home/jmorris/workspace/kagglecompetitions/titanic/R/')

# load data sets
test = read.csv('test.csv') # test set for submission
train = read.csv('train.csv') # training data


mtrain <- regexpr("[A-Z]\\w+\\.", train$Name, perl=TRUE)
mtest <- regexpr("[A-Z]\\w+\\.", test$Name, perl=TRUE)

train$Title <- regmatches(train$Name, mtrain)
  
test$Title <-  regmatches(test$Name, mtest) 
  

# imputation of age

age.model = lm(Age ~ Fare + as.factor(Title) + SibSp + Parch, data=train)
for(i in 1:nrow(train)){
  if(is.na(train[i, "Age"])){
    train[i, "Age"] = predict(age.model, newdata = train[i,])
  }
}

write.csv(train, "train_ageimputed.csv")

train_ageimputed <- read.csv("train_ageimputed.csv")

# logistic regression

model = glm(Survived ~ Pclass + Fare + SibSp + Parch + Sex + Age + Pclass:Sex + Age:Sex + SibSp:Sex, family = binomial(link = "logit"), data=train_ageimputed)
summary(model)

P = predict(model, newdata = train, type="response")
p.survive = round(P)

install.packages("caret")
require(caret)


confusionMatrix(p.survive, train_ageimputed$Survived)

# test data
for(i in 1:nrow(test)){
  if(is.na(test[i, "Age"])){
    test[i, "Age"] = predict(age.model, newdata = test[i,])
  }
}
write.csv(test, "test_ageimputed.csv")
test_ageimputed <- read.csv("test_ageimputed.csv")


test_ageimputed$Fare[153] <- mean(with(test_ageimputed, subset(Fare, Pclass == 3)), na.rm = TRUE)

p.survive = rep(NA, nrow(test_ageimputed))
for(i in 1:nrow(test_ageimputed)){
  P = predict(model, newdata = test_ageimputed[i,], type = "response")
  if(P <= 0.5){
   p.survive[i] = 0 
  }
  else{
    p.survive[i] = 1
  }
}

# or use the round function to assign zeros and ones (if 0.5 is the cutoff)
p.survive = round(predict(model, newdata = test_ageimputed, type = "response"))
head(p.survive, n = 100)


data = data.frame(PassengerId = test_ageimputed$PassengerId, survived = p.survive)
write.csv(data, "logisticregression.csv", row.names = FALSE)



