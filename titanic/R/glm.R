# Author: Borun Chowdhury
# Made based on http://vimeo.com/69269984



train=read.csv("train.csv")
attach(train)
names(train)<-tolower(names(train))

# Fill in missing ages based on linear model

age.model=lm(age~fare+sibsp+parch,data=train)

for(i in 1:nrow(train)) {
  if(is.na(train[i,"age"])){
    train[i,"age"]=predict(age.model,newdata=train[i,])
  }
}


# Make a glm model and used it to predict on the training set
model = glm(survived ~ pclass + fare + sibsp +parch+sex+age + pclass:sex + age:sex , family = binomial(link="logit"))
summary(model)
p= predict(model,newdata=train,type="response")
p.survived=round(p)

require(caret)
print(confusionMatrix(p.survived,survived))

# Read in test data

test.data=read.csv("test.csv")
names(test.data)<-tolower(names(test.data))

# Fill missing ages in test data using the same model used to fill in missing age data for training set

for(i in 1:nrow(test.data)) {
  if(is.na(test.data[i,"age"])){
    test.data[i,"age"]=predict(age.model,newdata=test.data[i,])
  }
}

# In the test data fare information is missing for the 153rd row. Fill it using average of pclass

test.data$fare[153]=mean(with(test.data,subset(fare,pclass==3)),na.rm=TRUE)

# predict survival and write to file

p.survive=round(predict(model,newdata=test.data, type="response"))
data=data.frame(PassengerId=test.data$passengerid,survived=p.survive)
write.csv(data,"submission-glm.csv",row.names=FALSE)
