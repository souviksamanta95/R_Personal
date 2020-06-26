install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

bank.df<-read.csv(file.choose(), header= T)
str(bank.df)
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# partition
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])

# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)


install.packages("rattle")
library(rattle)	
fancyRpartPlot(default.ct)


# predict
predict(default.ct,valid.df)


# set argument type = "class" in predict() to generate predicted class membership.

default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
library(caret)
library(ggplot2)
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

# generate confusion matrix for validation data
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))



### Tree Pruning

deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white')) 


# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table. 
printcp(cv.ct)

# prune by lower cp
pruned.ct <- prune(cv.ct,cp = 0.0169697)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

#
fancyRpartPlot(pruned.ct)

