install.packages("randomForest")
library(randomForest)
set.seed(121)
train.rf = randomForest(as.factor(HighSales) ~ ., data=Carseats.train, ntree=100, mtry=3, importance=TRUE)

#ntree= Number of trees to grow, mtry = Number of variables randomly sampled as candidates at each split, importance = Should importance of predictors be assessed?
 
train.rf


#obtaining class predictions
rf.classTrain <-  predict(train.rf, type="class")
head(rf.classTrain)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.train$HighSales),rf.classTrain)

#obtaining class predictions
rf.classTest <-  predict(train.rf,
                         newdata = Carseats.test,
                         type="class")
head(rf.classTest)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.test$HighSales),rf.classTest)

#Obtaining predicted probabilites for Test data
rf.probs=predict(train.rf,
                 newdata=Carseats.test,
                 type="prob")
head(rf.probs)

#Calculate ROC curve
rocCurve.rf <- roc(Carseats.test$HighSales,rf.probs[,"Yes"])
#plot the ROC curve
plot(rocCurve.rf,col=c(1))

#calculate the area under curve (bigger is better)
auc(rocCurve.rf)