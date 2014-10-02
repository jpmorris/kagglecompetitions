# kaggle Titanic 

setwd('/home/jmorris/workspace/kagglecompetitions/kaggle-titanicR')

# load data sets
test = read.csv('test.csv') # test set for submission
train = read.csv('train.csv') # training data

# split training for validation
library(caTools)
set.seed(3000)
split = sample.split(train$Survived, SplitRatio=0.7)
mytrain = subset(train, split==TRUE)
validation = subset(train, split==FALSE)


## RANDOM TREE

# build random tree model
library(rpart)
library(rpart.plot)

rtmodel <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=mytrain, method="class", control=rpart.control(minbucket=25))
prp(rtmodel)
PredictCART = predict(rtmodel, newdata=validation, type="class")

# model accuracy:
table(validation$Survived, PredictCART)
(151+62)/(151+14+41+62)# 79.5%

# baseline accuracy:
table(validation$Survived)
165/(165+103) # 61.6%

# ROC CURVE
library(ROCR)

PredictROC = predict(rtmodel, newdata=validation)
PredictROC
pred = prediction(PredictROC[,2], validation$Survived)
perf = performance(pred, "tpr", "fpr")
plot(perf)


## Boosting

## RANDOM FORESTS
library(randomForest)

rfmodel <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data=mytrain, na.action = na.roughfix)



## LOGISTIC REGRESSION
