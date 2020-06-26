mower.df<-read.csv(file.choose(), header= T)
str(mower.df)

set.seed(121)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])  
valid.index <- setdiff(row.names(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## new household
new.df <- data.frame(Income = c(60,40,33), Lot_Size = c(20,15,18))

# initialize normalized training, validation data, complete data frames to originals

train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df
new.norm.df<-new.df

library(caret)
library(ggplot2)

# normalization 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
train.norm.df$Income<-normalize(train.df$Income)
train.norm.df$Lot_Size<-normalize(train.df$Lot_Size)
valid.norm.df$Income<-normalize(valid.df$Income)
valid.norm.df$Lot_Size<-normalize(valid.df$Lot_Size)

new.norm.df$Income<-normalize(new.df$Income)
new.norm.df$Lot_Size<-normalize(new.df$Lot_Size)

# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
library(FNN)
nn <- knn(train = train.norm.df[, 1:2], test = valid.norm.df[,1:2], 
          cl = train.norm.df[,3], k = 5)
nn


# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2], 
                  cl = train.norm.df[,3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[,3])$overall[1] 
}
accuracy.df

#Accuracy plot
plot(accuracy.df, type="b", xlab="K- Value",ylab="Accuracy level")



#new data
knn.pred.new <- knn(train.norm.df[, 1:2], new.norm.df, 
                    cl = train.norm.df[, 3], k = 7)

knn.pred.new


