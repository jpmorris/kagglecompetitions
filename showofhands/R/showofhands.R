## INITIALIZATION ####################

# set working directory
setwd("/home/jmorris/workspace/learnR/analytics-edge/showofhands")
# set seed:
set.seed(144)
# load support functions
#source("showofhandsFunctions.R")

## LOAD DATA ########################

# load in training, test files -- https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/data
# THIS DATA HAS SPECIAL EMBARGO PROVISIONS, PLEASE READ: https://www.kaggle.com/c/the-analytics-edge-mit-15-071x/rules

trainRaw = read.csv("train.csv")
testRaw = read.csv("test.csv")

# dataset structure - variable definitions can be found in Quesitons.pdf (found on the kaggle website)
# note that the test set does not have the dependent variable in order to run ROC/AUC locally, we need
#       split the train file
str(trainRaw)
str(testRaw)
summary(trainRaw)
summary(testRaw)
# note that all the data is quantatative, nominal (non-ordinal) variables
# 110 variables - 4619 observations (samples)
# also note that there are missing data in the Year of Birth

## IMPUTATION #######################

# impute missing data:
# install.packages("mice")
library(mice)

trainImputed = trainRaw
testImputed = testRaw

drops <- c("Happy")
trainSimple = trainRaw[!(names(trainRaw) %in% drops)]
testSimple = testRaw[!(names(testRaw) %in% drops)]

trainSimpleImputed = complete(mice(trainSimple))
testSimpleImputed = complete(mice(testSimple))

trainImputed$YOB = trainSimpleImputed$YOB
testImputed$YOB = testSimpleImputed$YOB


## SPLIT LOCAL TRAIN/TEST DATA SET #####################

# the test set has no response variable 'Happy', the response is internal to kaggle, and is used to grade submission
# for this reason we need to split the training data in order to do comparision of techniques before submission
# such as ROC and AUC comparisions
library(caTools)
split = sample.split(trainImputed$Happy, SplitRatio = 0.7)
trainImputedTrain = subset(trainImputed, split==TRUE)
trainImputedTest = subset(trainImputed, split==FALSE)


# ALGORITHM COMPARISON ###############

# We want to predict 'Happy' from the other variables
# Let's review the different algorithm, the package and function to do them in r, and if they're appropriate for the problem at hand
# Classification algorigthms:
# Lionear Regression
# Logistic Regression
# Discriminant Analysis
# Naive Bayes
# Lasso
# Random Trees
# Random Forests
# Support Vector Machines

# for ROC curve
library(ROCR)
AUCValues <- numeric()
AUCLabels <- character()

# LOGISTIC REGRESSION ##############

require(ISLR)
# ROC/AUC
logisticROCModel = glm(Happy ~ Q107869No , data=trainImputed, family=binomial)
logisticROCTrainPrediction = predict(logisticROCModel,type="response")
logisticROCTestPrediction = predict(logisticROCModel, newdata=trainImputedTest, type="response")

logisticPredROCR = prediction(logisticROCTestPrediction, trainImputedTest$Happy)
logisticPerfROCR = performance(logisticPredROCR, "tpr", "fpr")
logisticAUC = performance(logisticPredROCR, "auc")@y.values
AUCValues <- c(AUCValues, as.numeric(logisticAUC))
AUCLabels <- c(AUCLabels, "Logistic")


# submission
logisticModel = glm(Happy~., data=trainImputed, family=binomial)
logisticTrainPrediction = predict(logisticModel,type="response")
logisticTestPrediction = predict(logisticModel, newdata=trainImputed, type="response")

#submission = data.frame(UserID = testImputed$UserID, Probability1 = logisticTestPrediction)
#write.csv(submission, "logisticRegression.csv", row.names=FALSE)


# RANDOM TREE ######################

library(rpart)
library(rpart.plot)

#ROC/AUC
treeROCModel = rpart(Happy~., data=trainImputedTrain, method="class")
treeROCTrainPrediction = predict(treeROCModel, type="prob")
treeROCTestPrediction = predict(treeROCModel, newdata=trainImputedTest, type="prob")[,2]


treePredROCR = prediction(treeROCTestPrediction, trainImputedTest$Happy)
treePerfROCR = performance(treePredROCR, "tpr", "fpr")
treeAUC = performance(treePredROCR, "auc")@y.values
AUCValues <- c(AUCValues, as.numeric(treeAUC))
AUCLabels <- c(AUCLabels, "Tree")

plot(treeROCModel)
text(treeROCModel)

# RANDOM FORESTS ####################

require(randomForest)
require(MASS)

forestROCModel = randomForest(Happy~.,data=trainImputedTrain)
forestROCTrainPrediction = predict(forestROCModel)
forestROCTestPrediction = predict(forestROCModel, newdata=trainImputedTest)

forestPredROCR = prediction(forestROCTestPrediction, trainImputedTest$Happy)
forestPerfROCR = performance(forestPredROCR, "tpr", "fpr")
forestAUC = performance(forestPredROCR, "auc")@y.values
AUCValues <- c(AUCValues, as.numeric(forestAUC))
AUCLabels <- c(AUCLabels, "Forest")


# BOOSTED TREES ###############

install.packages("gbm")
require(gbm)

boostROCModel = gbm(Happy~., data=trainImputedTrain, distribution="gaussian", n.trees=10000, shrinkage=0.01, interaction.depth=4)
boostROCTrainPrediction = predict(boostROCModel, n.trees=10000)
boostROCTestPrediction = predict(boostROCModel, n.trees=10000, newdata=trainImputedTest)

boostPredROCR = prediction(boostROCTestPrediction, trainImputedTest$Happy)
boostPerfROCR = performance(boostPredROCR, "tpr", "fpr")
boostAUC = performance(boostPredROCR, "auc")@y.values
AUCValues <- c(AUCValues, as.numeric(boostAUC))
AUCLabels <- c(AUCLabels, "Boost")


barplot(AUCValues, main="AUC of Different Methods", names.arg=AUCLabels)

