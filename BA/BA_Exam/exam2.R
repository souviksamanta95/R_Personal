# Reading the data from csv file
data2 <- read.csv("Diabetes.csv", header = T)
head(data2)
str(data2)
data2$Outcome <- factor(data2$Outcome)
set.seed(111)

# Partitioning the data into train and validation sets
train.index <- sample(row.names(data2), 0.6*dim(data2)[1])
valid.index <- setdiff(row.names(data2), train.index)
train.df <- data2[train.index, ]
valid.df <- data2[valid.index, ]

# Normalizing the data
train.norm.df <- train.df
valid.norm.df <- valid.df
data2.norm <- data2

library(caret)
norm.values <- preProcess(train.df[, 1:8], method = c("center", "scale"))
train.norm.df[, 1:8] <- predict(norm.values, train.df[, 1:8])
valid.norm.df[, 1:8] <- predict(norm.values, valid.df[, 1:8])
data2.norm[, 1:8] <- predict(norm.values, data2[, 1:8])

# Using KNN
library(FNN)
accuracy <- data.frame(k = seq(1,20,1), Accuracy = rep(0, 20))
for (i in 1:20){
 nn <- knn(train = train.norm.df[, 1:8], test = valid.norm.df[, 1:8],
          cl = train.norm.df[, 9], k = i)
 accuracy[i, 2] <- sum(nn==valid.norm.df$Outcome)/length(valid.norm.df$Outcome)
}
accuracy