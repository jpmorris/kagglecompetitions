train=read.csv("train.csv")
attach(train)
names(train)<-tolower(names(train))
age.model=lm(age~fare+sibsp+parch,data=train)


train[is.na(train$age),]$age=predict(age.model,newdata=train[is.na(train$age),])



model = qda(survived ~ pclass + sex + age + sibsp+ parch+fare+sibsp:parch +pclass:sex + age:sex , data=train )

p.survived= predict(model,newdata=train)$class


require(caret)
print(confusionMatrix(p.survived,train$survived))

test=read.csv("test.csv")
names(test)<-tolower(names(test))



test[is.na(test$age),]$age=predict(age.model,newdata=test[is.na(test$age),])


test$fare[153]=mean(with(test,subset(fare,pclass==3)),na.rm=TRUE)

p.survive=predict(model,newdata=test)$class
data=data.frame(PassengerId=test$passengerid,survived=p.survive)
write.csv(data,"submission-qda.csv",row.names=FALSE)