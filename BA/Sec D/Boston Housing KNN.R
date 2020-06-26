# Boston Housing 


housing.df<-read.csv(file.choose(), header= T)
str(housing.df)
summary(housing.df)


set.seed(12345)  
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])  
valid.index <- setdiff(row.names(housing.df), train.index)  
train.df <- housing.df[train.index, -14]
valid.df <- housing.df[valid.index, -14]


train.norm.df <- train.df
valid.norm.df <- valid.df


library(caret)
norm.values <- preProcess(train.df[, -13], method=c("center", "scale"))
train.norm.df[, -13] <- predict(norm.values, train.df[, -13])
valid.norm.df[, -13] <- predict(norm.values, valid.df[, -13])



library(ggplot2)
accuracy.df <- data.frame(k = seq(1, 15, 1), RMSE = rep(0, 5))

for(i in 1:15) {
  knn.pred <- class::knn(train = train.norm.df[,-13], 
                         test = valid.norm.df[,-13], 
                         cl = train.df$MEDV, k = i)
  accuracy.df[i, 2] <- RMSE(as.numeric(as.character(knn.pred)), valid.df$MEDV)
}                
accuracy.df

new.rec <- data.frame(0.2, 0, 7, 0, 0.538, 6, 62, 4.7, 4, 307, 21, 10)
names(new.rec) <- names(train.norm.df)[-13]
new.norm.rec <- predict(norm.values, new.rec)

knn.pred <- class::knn(train = train.norm.df[,-13], 
                       test = new.rec, 
                       cl = train.df$MEDV, k = 1)
knn.pred