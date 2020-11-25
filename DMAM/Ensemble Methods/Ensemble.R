#install.packages("ISLR")

library(psych)  
library(ggplot2)  
library(lattice)
library(caret)
library(rpart)  #for trees
library(rpart.plot)   # Enhanced tree plots
library(RColorBrewer) # Color selection for fancy tree plot
library(pROC)   #for ROC curves
library(ISLR)  #for the Carseat Data

#loading the data
data("Carseats")
#data structure
head(Carseats,10)
str(Carseats)
summary(Carseats)
summary(Carseats$Sales)


#creating new binary variable
Carseats$HighSales=ifelse(Carseats$Sales<=9.32,"No","Yes")

#remove old variable
Carseats$Sales <- NULL

str(Carseats)

#Splitting the data into training and test sets

datasample = sample(dim(Carseats)[1], dim(Carseats)[1]*0.7)

#create training and test data sets
Carseats.train = Carseats[datasample, ]
Carseats.test = Carseats[-datasample, ]

#check the distribution of HighSales
prop.table(table(Carseats.train$HighSales))
prop.table(table(Carseats.test$HighSales))

# install.packages('e1071')
library(e1071)
# define cross-validation (cv) parameters; we'll do 10-fold cross-validation
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.001))


#########################################
# Model 1
#########################################
set.seed(10)
train.tree <- train(HighSales ~ .,
                    data = Carseats.train,
                    method = "rpart",
                    control = rpart.control(minsplit = 10),
                    trControl = numFolds,
                    tuneGrid = cpGrid
)
train.tree
plot(train.tree)


#pruning
train.tree1 <- rpart(HighSales ~ ., data = Carseats.train, method = "class",
                     control = rpart.control(minsplit = 10, cp = 0.05))

print(train.tree1)
#install.packages("rattle")
library(rattle)
fancyRpartPlot(train.tree1)

#obtaining class predictions
tree.classTrain <-  predict(train.tree, type="raw")
head(tree.classTrain)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.train$HighSales),tree.classTrain)

#obtaining class predictions
tree.classTest <-  predict(train.tree,
                           newdata = Carseats.test,
                           type="raw")
head(tree.classTest)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.test$HighSales),tree.classTest)



#Obtaining predicted probabilites for Test data
tree.probs=predict(train.tree,
                   newdata=Carseats.test,
                   type="prob")
head(tree.probs)

#Calculate ROC curve
rocCurve.tree <- roc(Carseats.test$HighSales,tree.probs[,"Yes"])
#plot the ROC curve
plot(rocCurve.tree,col=c(4))

#calculate the area under curve (bigger is better)
auc(rocCurve.tree)

####################################################
#Model 2 : BAGGING
####################################################
cvcontrol <- trainControl(method="repeatedcv", number = 10,repeats = 10)


train.bagg <- train(as.factor(HighSales) ~ .,
                    data=Carseats.train,
                    method="treebag",
                    trControl=cvcontrol,
                    nbagg=100,
                    importance=TRUE)

plot(varImp(train.bagg))
#obtaining class predictions
bagg.classTrain <-  predict(train.bagg, type="raw")
head(bagg.classTrain)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.train$HighSales),bagg.classTrain)

#obtaining class predictions
bagg.classTest <-  predict(train.bagg,
                           newdata = Carseats.test,
                           type="raw")
head(bagg.classTest)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.test$HighSales),bagg.classTest)

#Obtaining predicted probabilites for Test data
bagg.probs=predict(train.bagg,
                   newdata=Carseats.test,
                   type="prob")
head(bagg.probs)

#Calculate ROC curve
rocCurve.bagg <- roc(Carseats.test$HighSales,bagg.probs[,"Yes"])
#plot the ROC curve
plot(rocCurve.bagg,col=c(6))

#calculate the area under curve (bigger is better)
auc(rocCurve.bagg)


############################################
#Model 3: BOOSTING ADABOOST
###########################################
install.packages("ada")
library(ada)
train.ada <- train(as.factor(HighSales) ~ .,
                   data=Carseats.train,
                   method="ada",
                   trControl=cvcontrol)

train.ada

#obtaining class predictions
ada.classTrain <-  predict(train.ada,type="raw")
head(ada.classTrain)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.train$HighSales),ada.classTrain)

#obtaining class predictions
ada.classTest <-  predict(train.ada,
                          newdata = Carseats.test,
                          type="raw")
head(ada.classTest)

#computing confusion matrix
confusionMatrix(as.factor(Carseats.test$HighSales),ada.classTest)

#Obtaining predicted probabilites for Test data
ada.probs=predict(train.ada,
                  newdata=Carseats.test,
                  type="prob")
head(ada.probs)

#Calculate ROC curve
rocCurve.ada <- roc(Carseats.test$HighSales,ada.probs[,"Yes"])
#plot the ROC curve
plot(rocCurve.ada, col=c(3))

#calculate the area under curve (bigger is better)
auc(rocCurve.ada)

################################################
#Model 4: Random Forest for classification trees
###############################################

install.packages("randomForest")
library(randomForest)
set.seed(121)
train.rf = randomForest(as.factor(HighSales) ~ ., data=Carseats.train, ntree=100, mtry=3, importance=TRUE)

#ntree= Number of trees to grow, mtry = Number of variables randomly sampled as candidates at each split, importance = Should importance of predictors be assessed?

train.rf
#mean( predict( train.rf ) != Carseats.train$HighSales )
#obtaining class predictions
rf.classTrain <-  predict(train.rf, type="response")
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


######
plot(rocCurve.tree,col=c(4))
plot(rocCurve.bagg,add=TRUE,col=c(6)) # color magenta is bagg
plot(rocCurve.rf,add=TRUE,col=c(1)) # color black is rf
plot(rocCurve.ada,add=TRUE,col=c(3)) # color green is ada