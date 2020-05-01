cat("\014")    # to clear console to CTRL+L
rm(list=ls())  

data=read.csv("Dataset/pharma108.csv")

df=data

names(df)
df=df[,-c(1,2)]

#data cleaning---------
Train_data$Years.of.Work[Train_data$Years.of.Work=="<NA>"]=0
Train_data$Years.of.Work[is.na(Train_data$Years.of.Work)] <- 0
Train_data$Years.of.Work[Train_data$Years.of.Work=="nA"]=0
Train_data$Years.of.Work[Train_data$Years.of.Work=="Na"]=0
Train_data$Years.of.Work[Train_data$Years.of.Work=="NA's"]=0

test_set$MonthsSince1stOrder[is.na(test_set$MonthsSince1stOrder)]=60

colnames(df)[colnames(df)=="PUS"] <- "y1" 			# Renaming the columns	
colnames(df)[colnames(df)=="NS"] <- "x1"			# Renaming the columns	
colnames(df)[colnames(df)=="CCE"] <- "x2"			# Renaming the columns	
colnames(df)[colnames(df)=="CCEUS"] <- "x3"			# Renaming the columns	
colnames(df)[colnames(df)=="ME"] <- "x4"			# Renaming the columns	
colnames(df)[colnames(df)=="AE"] <- "x5"			# Renaming the columns	
colnames(df)[colnames(df)=="TSME"] <- "x6"			# Renaming the columns	
colnames(df)[colnames(df)=="TSMEUS"] <- "x7"			# Renaming the columns	
colnames(df)[colnames(df)=="PBDITA"] <- "x8"			# Renaming the columns
colnames(df)[colnames(df)=="NW"] <- "x9"			# Renaming the columns
colnames(df)[colnames(df)=="TOL"] <- "x10"			# Renaming the columns
colnames(df)[colnames(df)=="TA"] <- "x11"			# Renaming the columns
colnames(df)[colnames(df)=="LNTA"] <- "x12"			# Renaming the columns
colnames(df)[colnames(df)=="LEV"] <- "x13"			# Renaming the columns
colnames(df)[colnames(df)=="RND"] <- "x14"			# Renaming the columns
colnames(df)[colnames(df)=="RNDUS"] <- "x15"			# Renaming the columns

#### SCATTER PLOT
ggplot(data = df, mapping = aes(x = x1, y = y1)) +
  geom_point(size=3, shape=20)+ggtitle("g") 
g=0
for(i in 1:ncol(df)-1){
  
  g=(ggplot(data = df, mapping = aes(x = df[,i+1], y = y1)) +
       geom_point(size=3, shape=20)+ggtitle("g" ))
  print(g)
}
df=df[,c(1,4,8,13,14,16)]

names(df)



library (tseries)

#jarque.bera.test(df$y1, robust = TRUE, method = c("chisq", "mc"),
#               N = 0, na.rm = FALSE)
jarque.bera.test(df$y1) 
jarque.bera.test(df$x3) 
jarque.bera.test(df$x7) 
jarque.bera.test(df$x13)
jarque.bera.test(df$x15)


reg<-lm(y1~x3+x7+x13+x15, data=df)        #Pharma108
##reg<-lm(y1~x3+x7+x12+x13+x15, data=df)        #Pharma108
summary(reg)


#### IDENTIFY MISSING & TREAT                                      # https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17

is.na(df)
sum(is.na(df))

#### IDENTIFY OUTLIERS & TREAT  

par(mfrow = c(1, 2))
boxplot(df)

boxplot(df)							# BOXPLOT:1.5*IQR
abline(h = min(df), col = "Blue")
abline(h = max(df), col = "Yellow")
abline(h = median(df), col = "Green")
abline(h = quantile(df$y1, c(0.25, 0.75)), col = "Red")
abline(h = quantile(df$x1, c(0.25, 0.75)), col = "Red")
summary(df)


# capOutlier <- function(y1){
#   #qnt <- quantile(df$y1,  probs=c(.25, .75), na.rm = T)
#   caps <- quantile(y1, probs=c(.05, .95), na.rm = T)
#   #H <- 1.5 * IQR(df$y1, na.rm = T)
#   #df$y1 <- ifelse (df$y1 < (qnt[1]-H),caps[1],df$y1)
#   #df$y1 <- ifelse (df$y1 > (qnt[2]+H),caps[2],df$y1)  
#   
#   y1 <- ifelse (y1 < caps[1],caps[1],df$y1)
#   y1 <- ifelse (y1 > caps[2],caps[2],df$y1)  
#   return(y1)
# }
# df$y1=capOutlier(df$y1)						# Replacing extreme values with percentiles


# outliers not needed-------
findoutliers <- function(col_name)
{
  nmiss <- sum(is.na(col_name))   #no. of missing values in dataset
  clean_col <- col_name[!is.na(col_name)]         # all non missing values will be assigned to a
  m <- mean(clean_col)
  n <- length(clean_col)
  s <- sd(clean_col)
  min <- min(clean_col)
  p1 <- quantile(clean_col,0.01)
  p99 <- quantile(clean_col,0.99)
  p2 <- quantile(clean_col,0.02)
  p98 <- quantile(clean_col,0.98)
  max <- max(clean_col)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag <- max>UC | min<LC
  return(c(n=n, nmiss=nmiss,
           outlier_flag=outlier_flag,
           mean=m,stdev=s, min=min,
           p1=p1, p99=p99,p2_=p2,p98_=p98, max=max,UC=UC, LC=LC))
}
findoutliers(df$y1)
df$y1[df$y1<(-0.6893363)]=-0.6893363

###Data split-------
library (caTools) 					 	# Require (caTools)
set.seed(101)            				 	# This is used to create same sample everytime
split1=sample.split(df$y1,SplitRatio=0.70)
train=subset(df,split1==TRUE)
# train
#View (train)
test=subset(df,split1==FALSE)

## Regression---------
m1<-lm(y1~x3+x7+x13+x15, data=train)       
summary(m1)




## Multicollinearity
#install.packages("sp")
#install.packages("raster")
library (sp)
library (raster)
library(usdm)
vif(df[-1]) 							            # calculates vif for the variables in fit

vc1<-vifcor(df[,-1], th=0.9) 					# identify collinear variables that should be excluded
vc1
vs1<-vifstep(df[,-1],th=10) 						# identify collinear variables that should be excluded
vs1
#df <-df[-c(1,2,3)] 						# Drop the column by index, dropping the first column 'Obs'
df1<-df

vif(df1[-c(1,2)])
View(df1)


## Heteroscedasticity Test

library (lmtest)							# White Test
bptest (df$y1~., data=df)
#bptest (df$crime~., data=df)
#bptest(reg, ~ y1*x1 + I(y1^2) + I(x1^2)+ I(y1*x2), data = df)	# Het. adjusted Regression: Applicable for a multiple regression


library (sandwich)							# Newey West Covariance Matrix Estimation

#NeweyWest(reg)								# Newey & West (1994) compute this type of estimator
#NeweyWest(reg, lag = 4, prewhite = FALSE)				# The Newey & West (1987) estimator requires specification of the lag 
# and suppression of prewhitening								
#kernHAC(reg, bw = bwNeweyWest)					# bwNeweyWest() can also be passed to kernHAC(), e.g.for the quadratic spectral kernel

ht<-coeftest(reg, df = Inf, vcov = NeweyWest(reg, lag = 4, prewhite = FALSE)) 
res<- resid (reg, df = Inf, vcov = NeweyWest(reg, lag = 4, prewhite = FALSE))
df1<-cbind(df,res)
bptest (df1$y1~res, data=df1,studentize = TRUE)



res<-resid(m1) 							#List of residuals
res
pred<-predict(m1)            					# fitted values
pred

## We now plot the residual against the observed values of the variable 

plot(train$y1, res, ylab="Residuals", xlab="x1", main="% of Vote") 
abline(0, 0)                  					# the horizon

## We now plot the pred against the actual values 

plot(train$y1, pred, ylab="y1-Est", xlab="x1", main="% of Vote") 
abline(0, 0)                  					# the horizon

library(gains) #for gains and lift chart
library(forecast) #for accuracy measures

# training
print(accuracy(m1$fitted.values, train$y1)[2]) #print RMSE
print(accuracy(m1$fitted.values, train$y1)[5]) #print MAPE

# validation
pred <- predict(m1, test)
View (pred)
print(accuracy(pred, test$y1)[2]) #print RMSE
print(accuracy(pred, test$y1)[5]) #print MAPE

summary(m1)


######Time Series---------
library(readxl)
myData <- read_excel("CSVs/petrol diesel data.xls")
df<- myData
View(df)

colnames(df)[colnames(df)=="Petrol"]<-"y1" 			    # Renaming the columns	
colnames(df)[colnames(df)=="Diesel"]<-"x1"			    # Renaming the columns	
colnames(df)[colnames(df)=="GDP"]   <-"x2"	        # Renaming the columns
colnames(df)[colnames(df)=="DieselPr"]<-"x3"	      # Renaming the columns
colnames(df)[colnames(df)=="PetrolPr"]<-"x4"	      # Renaming the columns

df <-df[-c(1,3)] 						                    # Drop the column by index, dropping the first column 'Obs'
View(df)

options (scipen=99999)
##Regression
reg<-lm(y1~x2+x3+x4, data=df)        #Petrol
summary(reg)

## Autocorrelation Check

library(lmtest)
dwtest (df$y1~.,data=df)				        # Durbin Watson Test

library (lmtest)
bgtest (df$y1~., data=df)			          # Breusch-Godfrey Test/LM Serial Correlation Test

install.packages("dplyr")
library (dplyr) 
res1<-lag(df1$res,1)
df1 <-cbind(df1, res1)
View(df1)

## Re-run regression after removing autocorrelation

reg2<-lm(y1~x2+x3+x4+res1, data=df1)
summary(reg2)
View(df1)

##Re-test Autocorrelation
library (lmtest)
df1<-df1[-5]
bgtest (df1$y1~., data=df1)				        # Breusch-Godfrey Test/LM Serial Correlation Test
View(df1)


train_ins = df[1:720,]						# In-sample
test_oos  = df[721:900,]					# Out of Sample	 

##Regression
reg<-lm(y1~x2+x3+x4, data=train_ins)        #Petrol
summary(reg)

## Multicollinearity
install.packages("sp")
install.packages("raster")
install.packages("usdm")
library (sp)
library (raster)
library(usdm)
df=df[,c(1:4)]
vif(df) 							# calculates vif for the variables in fit

vc1<-vifcor(df[,-1], th=0.9) 					# identify collinear variables that should be excluded
vc1
vs1<-vifstep(df[,-1],th=10) 						# identify collinear variables that should be excluded
vs1
df <-df[-c(1,2,3)] 						# Drop the column by index, dropping the first column 'Obs'

vif(df1[-c(1,3)])

View(df) 

##Re-run Regression
reg<-lm(y1~x2+x3+x4, data=df)        #Petrol
res<-resid(reg) 							      #List of residuals
df1 <-cbind(df, res)
View(df1)
summary(reg)

# training
print(accuracy(m1$fitted.values, train$y1)[2]) #print RMSE
print(accuracy(m1$fitted.values, train$y1)[5]) #print MAPE

# validation
pred <- predict(m1, test)
View (pred)
print(accuracy(pred, test$y1)[2]) #print RMSE
print(accuracy(pred, test$y1)[5]) #print MAPE
