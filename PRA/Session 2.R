# Session 2 - 7 July 2020 - 10:15 am

##Import the data

##Check for class bias
##Create training and test samples
##Compute information value to find out important variables
##Build logit models and predict on test data
##Do model diagnostics

###Import data
myData <- read.csv("D:/Analytics/R/PRA/Datasets/bankloan.csv", header = T)		# Select the file 
def.df<-myData
nrow(def.df)
ncol(def.df)
head(def.df)
View(def.df)
str(def.df)
def.df$ed <- factor(def.df$ed)
def.df$default <- factor(def.df$default)

#### DESCRIPTIVE STATS
summary(def.df)
quantile (def.df$age,      c(0.01, 0.05,0.95,0.99))			# QUARTILE
quantile (def.df$ed,       c(0.01, 0.05,0.95,0.99))
quantile (def.df$employ,   c(0.01, 0.05,0.95,0.99))
quantile (def.df$address,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$income,   c(0.01, 0.05,0.95,0.99))
quantile (def.df$debtinc,  c(0.01, 0.05,0.95,0.99))
quantile (def.df$creddebt, c(0.01, 0.05,0.95,0.99))
quantile (def.df$othdebt,  c(0.01, 0.05,0.95,0.99))

# Pearsonian Correlation Coefficent
cor(def.df[-c(1,3,10)])  	
#### OUTLIERS TREATMENT, CAPPING OUTLIERS    
capOutlier <- function(y1){
  #qnt <- quantile(df$age,  probs=c(.25, .75), na.rm = T)
  caps <- quantile(def.df$age, probs=c(.05, .95), na.rm = T)
  def.df$age <- ifelse (def.df$age < caps[1],caps[1],def.df$age)
  def.df$age <- ifelse (def.df$age > caps[2],caps[2],def.df$age)  
  return(y1)
}


age_adj=capOutlier(def.df$age)						# Replacing extreme values with percentiles
age_adj<-data.frame(age_adj) 
def.df<-cbind(def.df,age_adj)
View(def.df)

which(def.df$age != def.df$age_adj)

############ NEED FUNCTION CORRECTION

capOutlier <- function(y1){
  #qnt <- quantile(df$age,  probs=c(.25, .75), na.rm = T)
  caps <- quantile(y1, probs=c(.05, .95), na.rm = T)
  y1 <- ifelse (y1 < caps[1],caps[1],y1)
  y1 <- ifelse (y1 > caps[2],caps[2],y1)  
  return(y1)
}


age_adj2=capOutlier(def.df$age)						# Replacing extreme values with percentiles
age_adj2<-data.frame(age_adj2) 
def.df<-cbind(def.df,age_adj2)
#View(def.df)
which(def.df$age != def.df$age_adj2)


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
# install.packages("usdm")
# install.packages("regclass")
library(regclass)
library(usdm)
# VIF(def.df[-c(1,9)])    **** Not applicable to data frames
VIF(logitMod)
#### Build Logit Models and Predict
options(scipen=99999)
logitMod <- glm(default ~ age + ed + employ + address + debtinc, data=trainingData, family=binomial(link="logit"))
####Model Diagnostics
summary(logitMod)
predicted.training <- plogis(predict(logitMod, trainingData))  # predicted scores
predicted.test <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted.training <- predict(logitMod, trainingData, type="response")  # predicted scores
predicted.training
predicted.test <- predict(logitMod, testData, type="response")  # predicted scores
predicted.test






















