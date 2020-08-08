rm(list=ls())
cat("\014")

###Import data
myData <- read.csv("D:/Analytics/R/PRA/Datasets/CreditCardDefault.csv", header = T)		# Read the file		
def.df<-myData
nrow(def.df)
ncol(def.df)
head(def.df)
View(def.df)
str(def.df)
#def.df$ed <- sapply(def.df$ed, as.character)
def.df$SEX<-factor(def.df$SEX)
def.df$EDUCATION<-factor(def.df$EDUCATION)
def.df$MARRIAGE<-factor(def.df$MARRIAGE)
def.df$PAY_0<-factor(def.df$PAY_0)
def.df$PAY_2<-factor(def.df$PAY_2)
def.df$PAY_3<-factor(def.df$PAY_3)
def.df$PAY_4<-factor(def.df$PAY_4)
def.df$PAY_5<-factor(def.df$PAY_5)
def.df$PAY_6<-factor(def.df$PAY_6)
def.df$default.payment.next.month<-factor(def.df$default.payment.next.month)


str(def.df)


#### DESCRIPTIVE STATS
summary(def.df)
quantile (def.df$AGE,      c(0.01, 0.05,0.95,0.99))			# QUARTILE
quantile (def.df$BILL_AMT1,   c(0.01, 0.05,0.95,0.99))
quantile (def.df$BILL_AMT2,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$BILL_AMT3,   c(0.01, 0.05,0.95,0.99))
quantile (def.df$BILL_AMT4,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$BILL_AMT5, c(0.01, 0.05,0.95,0.99))
quantile (def.df$BILL_AMT6,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT1,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT2,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT3,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT4,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT5,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$PAY_AMT6,  c(0.01, 0.05,0.95,0.99))

table(def.df$SEX)
table(def.df$EDUCATION)
table(def.df$MARRIAGE)
table(def.df$PAY_0)
table(def.df$PAY_2)
table(def.df$PAY_3)
table(def.df$PAY_4)
table(def.df$PAY_5)
table(def.df$PAY_6)
table(def.df$default.payment.next.month)

cor(def.df[c(2,6,13,14,15,16,17,18,19,20,21,22,23,24)])  						# Pearsonian Correlation Coefficent

#### OUTLIERS TREATMENT, CAPPING OUTLIERS    
capOutlier <- function(y1){
  #qnt <- quantile(df$age,  probs=c(.25, .75), na.rm = T)
  caps <- quantile(y1, probs=c(.05, .95), na.rm = T)
  y1 <- ifelse (y1 < caps[1],caps[1],y1)
  y1 <- ifelse (y1 > caps[2],caps[2],y1)  
  return(y1)
}

age_adj <- capOutlier(def.df$age)						# Replacing extreme values with percentiles
income_adj <- capOutlier(def.df$income)
age_adj<-data.frame(age_adj) 
income_adj<-data.frame(income_adj) 
def.df <- cbind(def.df,age_adj)
def.df <- cbind(def.df,income_adj)
str(def.df)


##Check Class bias
table(def.df$default)

##Create Training and Test Samples

# Create Training Data
input_ones  <- def.df[which(def.df$default == 1), ]  # all 1's
input_zeros <- def.df[which(def.df$default == 0), ]  # all 0's
set.seed(100)                                        # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.8*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.4*nrow(input_zeros))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's
table(trainingData$default)

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 
table(testData$default)

###VIF: Multicollinearity
#install.packages("usdm")
library(usdm)
vif(def.df[-c(1,3,10)])

#### Build Logit Models and Predict
options(scipen=99999)
logitMod <- glm(default ~  age + ed + employ + address + debtinc, data=trainingData, family=binomial(link="logit"))
####Model Diagnostics
summary(logitMod)

predicted.training <- plogis(predict(logitMod, trainingData))  # predicted scores
predicted.test <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted.training <- predict(logitMod, trainingData, type="response")  # predicted scores
predicted.test <- predict(logitMod, testData, type="response")  # predicted scores

###Decide on optimal prediction probability cutoff for the model
#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(trainingData$default, predicted.training)[1] 
optCutOff <- optimalCutoff(testData$default, predicted.test)[1] 
optCutOff
#=> 0.71


###Misclassification Error
#misClassError(trainingData$default, predicted.training, threshold = optCutOff)
misClassError(trainingData$default, predicted.training, threshold = 0.5)
misClassError(testData$default, predicted.test, threshold = 0.5)
#misClassError(testData$default, predicted.test, threshold = optCutOff)


###Concordance
Concordance(trainingData$default, predicted.training)
Concordance(testData$default, predicted.test)

###Specificity and Sensitivity
##Sensitivity=(# Actual 1's and Predicted as 1's)/(# of Actual 1's) or True positive response or TPR
sensitivity(trainingData$default, predicted.training, threshold = 0.5)
#sensitivity(trainingData$default, predicted.training, threshold = optCutOff)
sensitivity(testData$default, predicted.test, threshold = 0.5)
#sensitivity(testData$default, predicted.test, threshold = optCutOff)

##Specificity=(# Actual 0's and Predicted as 0's)/(# of Actual 0's)
specificity(trainingData$default, predicted.training, threshold = 0.5)
#specificity(trainingData$default, predicted.training, threshold = optCutOff)
specificity(testData$default, predicted.test, threshold = 0.5)
#specificity(testData$default, predicted.test, threshold = optCutOff)


##Confusion Matrix
confusionMatrix(trainingData$default, predicted.training, threshold = 0.5)
#confusionMatrix(trainingData$default, predicted.training, threshold = optCutOff)
confusionMatrix(testData$default, predicted.test, threshold = 0.5)
#confusionMatrix(testData$default, predicted.test, threshold = optCutOff)

###ROC: Receiver Operating Characteristics Curve: 
##Receiver Operating Characteristics Curve gives the percentage of true positives 
##accurately predicted by a logit model as the prediction probability cutoff decreases 
##from 1 to 0. For a good model, as the cutoff is lowered,it  
##marks more of actual 1's as positives and lesser of actual 0's as 1's. 
##So for a good model, the curve should rise steeply, 
##indicating that the TPR (Y-Axis) increases faster than the FPR (X-Axis) as 
##the cutoff score decreases. 
##Greater the area under the ROC curve, better the predictive ability of the model.
plotROC(trainingData$default, predicted.training)
plotROC(testData$default, predicted.test)

