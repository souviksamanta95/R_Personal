#MBA - Business Analytics Class - Demonstrated on 07-Mar-2020
#code for Naïve Bayes
#Use iris Data Set
library(e1071)
ir=iris
View(ir)
train=ir[1:100,]
View(train)
test=ir[101:150,]
View(test)
model=naiveBayes(Species~.,data = train)
pred = predict(model,test)
table(pred)
table(test$Species)

#Use Disease_Pred Data Set

Disease=read.csv("Disease_Pred.csv")
nrow(Disease)
train=Disease[1:450,]
View(train)
test=Disease[451:569,]
View(test)
head(test)
levels(train$diagnosis)
model=naiveBayes(diagnosis~.,data = train)
class(model)
pred = predict(model,test)
table(pred)
table(test$diagnosis)
##########################
#Confusion Matrix
table(test$diagnosis,pred)

#Diagonal from left to right is correct value(85 to 25)
#Diagonal from right to left is wrong value (2-7)

#to get percentage install scales
install.packages("scales")
library(scales)
accuracy=percent((85+25)/119)
accuracy

#Fliping the data - two levels of classifications
ir1=ir[(sample(nrow(ir))),]
View(ir1)
train=ir1[1:100,]
View(train)
test=ir1[101:150,]
View(test)
model=naiveBayes(Species~.,data = train)
pred = predict(model,test)
table(pred)
table(test$Species)
