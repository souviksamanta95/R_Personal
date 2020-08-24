## This file gives code to 
## Read a csv file ##Create a Graph ## Summary of data 
## Identify Missing and its treatment
## Identify Outliers and its treatment
## DataSplit for a cross-section, TRAIN:TEST and time-series data: In-sample:Out-of-sample
## Make functional transformations and Graphs for the 'Train (In-sample)' data
## Run a Bivariate Regression Model for each functional transformation
## Run the regression model
## Check Regression Diagnostics: Multicollineariry, Hetroscedasticity(cross-section data) and Autocorrelation (time-series data)
## Re-estimating a model after adjusting for Heteroscedasticity and Autocorrelation

###### READ THE FILE

myFile <- file.choose()                    			# Select the file 
myData  <- read.csv (myFile,header=TRUE)   			# Read the file		
#vote<-myData				   			# Reading VOTE and EXPN_SHARE data: this line has # to be changed for your file
CM<-myData				   			# Reading VOTE and EXPN_SHARE data: this line has # to be changed for your file
#df<-vote				   			# Renaming the datafile: This line has to be changed		
df<-CM
colnames(df)[colnames(df)=="CM"] <- "y1" 			# Renaming the columns	
colnames(df)[colnames(df)=="FLR"] <- "x1"			# Renaming the columns	
colnames(df)[colnames(df)=="PGNP"] <- "x2"			# Renaming the columns	
colnames(df)[colnames(df)=="TFR"] <- "x3"			# Renaming the columns	
#df <-df[-c(1)] 						# Drop the column by index, dropping the first column 'Obs'
View(df)


##### PLOT
plot(y1~x1,data=df)				        	# Simple plot

#### ggplot2
library(ggplot2)						# Require ggplot2

#### SCATTER PLOT
ggplot(data = df, mapping = aes(x = x1, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("CM Vs FLR") 

ggplot(data = df, mapping = aes(x = x2, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("CM Vs PGNP") 

ggplot(data = df, mapping = aes(x = x3, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("CM Vs TFR") 
#### DESCRIPTIVE STATS
summary(df)
quantile (df$y1,  c(0.01, 0.05,0.95,0.99))			# QUARTILE
quantile (df$x1,  c(0.01, 0.05,0.95,0.99))
quantile (df$x2,  c(0.01, 0.05,0.95,0.99))
quantile (df$x3,  c(0.01, 0.05,0.95,0.99))
cor(df)  							# Pearsonian Correlation Coefficent

####Test of Normality

#install.packages("tseries")
library (tseries)

#jarque.bera.test(df$y1, robust = TRUE, method = c("chisq", "mc"),
#               N = 0, na.rm = FALSE)
jarque.bera.test(df$y1) 
jarque.bera.test(df$x1) 
jarque.bera.test(df$x2) 
jarque.bera.test(df$x3) 

#### IDENTIFY MISSING & TREAT                                      # https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17

is.na(df)
sum(is.na(df))
mean(is.na(df))
df$y1[df$y1 == 999] <- NA					# In case the unavailable values reported as '999' or something else, convert as NA

####List of R Packages MICE, Amelia, missForest, Hmisc, mi
#install.packages("missForest")
library (missForest)
data ("df")
df.mis <- prodNA(df, noNA = 0.1)				# Generate 10% missing values at Random 
View(df.mis)					        # Check missing values introduced in the data


df.imp <- missForest(df.mis)  #impute missing values, using all parameters as default values
View(df.imp)
df.imp$ximp          #check imputed values

df.imp$OOBerror     #check imputation error, NRMSE

#comparing actual data accuracy
df.err <- mixError(df.imp$ximp, df.mis, df)
df.err



#### IDENTIFY OUTLIERS & TREAT  

par(mfrow = c(1, 2))
boxplot(df)

boxplot(df)							# BOXPLOT:1.5*IQR
abline(h = min(df), col = "Blue")
abline(h = max(df), col = "Yellow")
abline(h = median(df), col = "Green")
abline(h = quantile(df$y1, c(0.25, 0.75)), col = "Red")
abline(h = quantile(df$x1, c(0.25, 0.75)), col = "Red")

#### OUTLIERS TREATMENT, CAPPING OUTLIERS    
capOutlier <- function(y1){
  #qnt <- quantile(df$y1,  probs=c(.25, .75), na.rm = T)
  caps <- quantile(df$y1, probs=c(.05, .95), na.rm = T)
  #H <- 1.5 * IQR(df$y1, na.rm = T)
  #df$y1 <- ifelse (df$y1 < (qnt[1]-H),caps[1],df$y1)
  #df$y1 <- ifelse (df$y1 > (qnt[2]+H),caps[2],df$y1)  
  
  df$y1 <- ifelse (df$y1 < caps[1],caps[1],df$y1)
  df$y1 <- ifelse (df$y1 > caps[2],caps[2],df$y1)  
  return(y1)
}
df$y1=capOutlier(df$y1)						# Replacing extreme values with percentiles

capOutlier <- function(x1){
  
  caps <- quantile(df$x1, probs=c(.05, .95), na.rm = T)
  
  df$x1 <- ifelse (df$x1 < caps[1],caps[1],df$x1)
  df$x1 <- ifelse (df$x1 > caps[2],caps[2],df$x1)  
  return(x1)
}
df$x1=capOutlier(df$x1)						# Replacing extreme values with percentiles

capOutlier <- function(x2){
  caps <- quantile(df$x2, probs=c(.05, .95), na.rm = T)
  df$x2 <- ifelse (df$x2 < caps[1],caps[1],df$x2)
  df$x2 <- ifelse (df$x2 > caps[2],caps[2],df$x2)  
  return(x2)
}
df$x2=capOutlier(df$x2)						# Replacing extreme values with percentiles

capOutlier <- function(x3){
  caps <- quantile(df$x3, probs=c(.05, .95), na.rm = T)
  df$x3 <- ifelse (df$x3 < caps[1],caps[1],df$x3)
  df$x3 <- ifelse (df$x3 > caps[2],caps[2],df$x3)  
  return(x3)
}
df$x3=capOutlier(df$x3)						# Replacing extreme values with percentiles

View (df)
View (CM)

#### Cross-Section Data: Random Split: TRAIN AND TEST (70:30)

library (caTools) 					 	# Require (caTools)
set.seed(101)            				 	# This is used to create same sample everytime
split1=sample.split(df$y1,SplitRatio=0.70)
train=subset(df,split1==TRUE)
# train
View (train)
test=subset(df,split1==FALSE)
# test 	
View (test)

#### Time-Series Data: Split into In-sample and Out-of-Sample

myFile <- file.choose()                    			# Select the file 
myData  <- read.csv  (myFile,header=TRUE)   			# Read the file	
df 	<-myData	
#	train_ins <- read.csv(myData,nrows=2000)			# Creating a file with firt 2000 rows: In-sample 
train_ins = df[1:720,]						# In-sample
test_oos  = df[721:900,]					# Out of Sample	 

#### Work on the Development/Train Data
#### Plot, transform, Run Regression, Store the best fit (maximum of R2)

### Linear Function
library (ggplot2)
 ggplot(data = train, mapping = aes(x = x2, y = y1)) +
  geom_point(size=3, shape=20)+
  ggtitle("Linear")

## Regression
m1=lm(y1~x2,data=train)
summary(m1) 

# Above steps are repeated for each independent variable for different other functional forms

### Quadratic Function

x1.2 <-train$x1**2
x1.2
cbind(train, x1.2) 						# adding a new column 

library (ggplot2)						
ggplot(data = train, mapping = aes(x = x1.2, y = y1)) +
  geom_point(size=3, shape=20)+ ggtitle("Quadratic") 

m2=lm(y1~x1.2,data=train)
summary(m2) 


### Natural Log

x1.3 <-log(train$x2)
x1.3

cbind(train, x1.3)
jarque.bera.test(x1.3)
library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.3, y = y1)) +
  geom_point(size=3, shape=20)+ ggtitle("Natural log") 



m3=lm(y1~x1.3,data=train)
summary(m3)

### Exponential

x1.4 <-exp(-train$x1)
x1.4

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.4, y = y1)) +
  geom_point(size=3, shape=20)+ ggtitle("Exponential(-ve)") 

m4=lm(y1~x1.4,data=train)
summary(m4)

### Square-Root

x1.5 <-sqrt(train$x1)
x1.5

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.5, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("Square Root") 

m5=lm(y1~x1.5,data=train)
summary(m5)

### Factorial

x1.6 <-factorial(train$x1)
x1.6

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.6, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("Factorial") 

m6=lm(y1~x1.6,data=train)
summary(m6)

### Inverse/Reciprocal

x1.7 <-(1/(train$x2))
x1.7

library(tseries)
jarque.bera.test(x1.7)

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.7, y = y1)) +
  geom_point(size=3, shape=20)+ ggtitle("Reciprocal") 

m7=lm(y1~x1.7,data=train)
summary(m7)

### Trigonometric Functions

x1.8 <-cos(train$x1)
x1.8

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.8, y = y1)) +
  geom_point(size=3, shape=20)+
  ggtitle("Trigonometric") 

m8=lm(y1~x1.8,data=train)
summary(m8)

### Power Functions

x1.9 <-10**(train$x1)
x1.9

library (ggplot2)
ggplot(data = train, mapping = aes(x = x1.9, y = y1)) +
  geom_point(size=3, shape=20)+ ggtitle("Power") 

m9=lm(y1~x1.9,data=train)
summary(m9)


#### Regression on TRAIN data, after choosing appropriate transformation of variables

reg=lm(y1~x1+x1.3+x3,data=train)
summary(m1)
res<-resid(m1) 							#List of residuals
res
pred<-predict(m1)            					# fitted values
pred

## We now plot the residual against the observed values of the variable 

plot(train$y1, res, ylab="Residuals", xlab="x1", main="Residuals") 
abline(0, 0)                  					# the horizon

## We now plot the pred against the actual values 

plot(train$y1, pred, ylab="y1-Est", xlab="x1", main="CM") 
abline(0, 0)                  					# the horizon


## Multicollinearity
library(usdm)
vif(train) 							# calculates vif for the variables in fit
#vc1<-vifcor(train, th=0.9) 					# identify collinear variables that should be excluded
#vc1
vs1<-vifstep(train,th=10) 						# identify collinear variables that should be excluded
vs1

## Heteroscedasticity Test

library (lmtest)							# White Test
bptest (train$y1~., data=train)
# bptest(m1, ~ y*x1 + I(y^2) + I(x1^2)+ I(y*x2)+.., data = train)	# Het. adjusted Regression: Applicable for a multiple regression


library (sandwich)							# Newey West Covariance Matrix Estimation

NeweyWest(m1)								# Newey & West (1994) compute this type of estimator

NeweyWest(m1, lag = 4, prewhite = FALSE)				# The Newey & West (1987) estimator requires specification of the lag 
# and suppression of prewhitening								

kernHAC(m1, bw = bwNeweyWest)					# bwNeweyWest() can also be passed to kernHAC(), e.g.for the quadratic spectral kernel

library (sandwich)							# Newey West Adjusted Regression Estimation
coeftest(m1, df = Inf, vcov = NeweyWest(m1, lag = 4, prewhite = FALSE)) 
coeftest(m1, df = Inf, vcov = NeweyWest) 

## Autocorrelation Check

library(lmtest)
dwtest (train$y1~.,data=train)				        # Durbin Watson Test

library (lmtest)
bgtest (train$y1~., data=train)				        # Breusch-Godfrey Test/LM Serial Correlation Test


#stepwise regression with exhaustive search
install.packages("leaps")
library(leaps)

search <- regsubsets(y1 ~ .,
                     data = train,
                     nbest = 1,
                     nvmax = dim(train)[2],
                     method = "forward") 

sum <- summary(search)
View(sum)
View(search)


#To find the top three models, the criteria used is as follows:
#Find the 3 highest values of adjusted R-squared
t(t(sum$adjr2))
# top 3 models
models <-  order(sum$adjr2, decreasing = T)[1:3]
models

library(gains) #for gains and lift chart
library(forecast) #for accuracy measures

par(mfcol=c(1,3))
for (model in models){
  print(model) #print model number
  selected.vars = names(train)[sum$which[model,]]
  reg.model <- lm(y1 ~ ., data = train [,selected.vars])}
selected.vars


step.forward <- step(reg2, direction = "forward")
summary(step.forward)
step.backward <- step(reg2, direction = "backward")
#or step.backward <- step(reg2)
summary(step.backward)
#predictions on validation set
lm1 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO 
          + LSTAT, data = train.df)
pred1.valid <- predict(lm1, data = valid.df)
accuracy(pred1.valid, valid.df$MEDV)
step.both <- step(reg2, direction = "both")
summary(step.both)
#predictions on validation set
lm3 <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + 
            LSTAT, data = train.df)
pred3.valid <- predict(lm3, data = valid.df)
accuracy(pred3.valid, valid.df$MEDV)



## Re-run regression after removing multicollinearity and autocorrelation
#m1=lm(y~.+res1, data=train)
#summary(m1)
#res<-resid(m1) 							# List of residuals
#res  

### Validation/TEST (Cross-Section Data), Out-of-Sample (Time-Series Data)

### Forecast and Error  

