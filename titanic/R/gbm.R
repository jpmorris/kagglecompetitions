# Author: Borun Chowdhury
# Based on http://vimeo.com/71992876

require(gbm)
require(dplyr)

# read in train and test data
train=read.csv("train.csv")
test=read.csv("test.csv")

# extract survived for train data in separate vector and comine train and test to act on them together for data transformations

survived=train$Survived
train=select(train,-Survived)
end_trn=nrow(train)
all=rbind(train,test)
end=nrow(all)

all=select(all,Pclass,Sex,Age,SibSp,Parch,Fare, Embarked)

ntrees=5000

# fit gbm model
Model=gbm.fit(
  x=all[1:end_trn,],y=survived,distribution="bernoulli",n.trees=ntrees
  ,shrinkage=0.01
  ,interaction.depth=3
  ,n.minobsinnode=10
  ,nTrain=round(end_trn*.8)
  ,verbose=TRUE
  
  )

# predict on test and train sets
TestPredictions = predict(object=Model, newdata=all[(end_trn+1):end,],n.trees=gbm.perf(Model,plot.it=FALSE),type="response")
TrainPredictions = predict(object=Model, newdata=all[1:end_trn,],n.trees=gbm.perf(Model,plot.it=FALSE),type="response")

# round up to get survival boolean values from probabilities

TestPredictions=round(TestPredictions)
TrainPredictions=round(TrainPredictions)

# optionally print confusion matrix on train data using previously stored survival values
# print(confusionMatrix(TrainPredictions,survived))

# save data
data=data.frame(PassengerId=test$PassengerId,survived=TestPredictions)
write.csv(data,"submission-gbm.csv",row.names=FALSE)
