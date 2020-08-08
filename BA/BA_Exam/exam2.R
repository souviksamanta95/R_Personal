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
accuracy <- data.frame(k = seq(1,100,1), Accuracy = rep(0, 100))
for (i in 1:100){
 nn <- knn(train = train.norm.df[, 1:8], test = valid.norm.df[, 1:8],
          cl = train.norm.df[, 9], k = i)
 accuracy[i, 2] <- sum(nn==valid.norm.df$Outcome)/length(valid.norm.df$Outcome)
}
plot(accuracy)

acc <- accuracy$Accuracy


plot(acc, type = "b" , col = "light green",cex = 1, pch = 20,
     xlab = "k(Number Of Neighbours)",ylab = "Classification accuracy( in %)",
     main = "Accuracy vs Number of Neighbours")
abline(v=which(acc==max(acc)), col = "red", lwd = 1.5)
abline(h=max(acc), col = "light blue", lty = 3)
abline(h=min(acc), col = "light blue", lty = 3)
