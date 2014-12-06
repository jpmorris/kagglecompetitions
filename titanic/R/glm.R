train=read.csv("train.csv")
attach(train)
names(train)<-tolower(names(train))
age.model=lm(age~fare+sibsp+parch,data=train)


train[is.na(train$age),]$age=predict(age.model,newdata=train[is.na(train$age),])


model = glm(survived ~ pclass + fare + sibsp +parch+sex+age + pclass:sex + age:sex , family = binomial(link="logit"))

p= predict(model,newdata=train,type="response")
p.survived=round(p)

require(caret)
print(confusionMatrix(p.survived,survived))

test=read.csv("test.csv")
names(test)<-tolower(names(test))



test[is.na(test$age),]$age=predict(age.model,newdata=test[is.na(test$age),])


test$fare[153]=mean(with(test,subset(fare,pclass==3)),na.rm=TRUE)

p.survive=round(predict(model,newdata=test, type="response"))
data=data.frame(PassengerId=test$passengerid,survived=p.survive)
write.csv(data,"submission-glm.csv",row.names=FALSE)