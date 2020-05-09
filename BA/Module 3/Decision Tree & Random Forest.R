install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")
View(mower.df)

class.tree <- rpart(Ownership ~., data = mower.df, 
                    control = rpart.control(maxdepth = 2), method = "class")

prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10) 


install.packages("prp")

library(rpart)
library(rpart.plot)

bank.df <- read.csv("UniversalBank.csv")
View(bank.df)
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# partition
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
View(default.ct)



install.packages("party")
#Read data file
data <- read.csv("Cardiotocographic.csv")
#Create new column as Factor of NSP
str(data)
data$NSPF <- as.factor(data$NSP)

set.seed(1234)
pd <- sample(2, nrow(data), replace=TRUE, prob = c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]

#Decision tree with party
library(party)
library(rpart.plot)
tree <-ctree(NSPF~LB+AC+FM, data = train, controls=ctree_control(mincriterion=0.9, minsplit=50))
print(tree)


# plot(tree,type="simple")



#Random Forest -

# Read Data
data <- read.csv("CTG.csv", header = TRUE)
str(data)

#Convert NSP as factor
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
#Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be reproduced
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]



# Random Forest

install.packages("randomForest")
library(randomForest)
set.seed(222)
rf <- randomForest(NSP~.,data=train)
rf <- randomForest(NSP~., data=train, ntree = 300, mtry = 8, importance = TRUE, proximity = TRUE)
print(rf)

#Interpretation
#Error Estimate - 5.75 means 95% accuracy
attributes(rf)

rf$confusion
rf$type
rf$votes

# Prediction & Confusion Matrix - train data
install.packages("caret")
library(caret)
p1 <- predict(rf, train)
head(p1)
head(train$NSP)

confusionMatrix(p1, train$NSP)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)





# Error rate of Random Forest
plot(rf)

# Tune mtry
t <- tuneRF(train[,-22], train[,22],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")

# Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")
importance(rf)
varUsed(rf)

# Partial Dependence Plot
partialPlot(rf, train, ASTV, "2")

# Extract Single Tree
getTree(rf, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$NSP)
