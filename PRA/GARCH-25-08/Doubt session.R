rm(list=ls())
cat("\014")
dev.off()

# install.packages("rugarch")
# install.packages("MTS")
# install.packages("forecast")
# install.packages("tseries")

library (rugarch)
library (MTS)
library(tseries)
library(forecast)

#Import Data
options (scipen=99999)
setwd("D:/Analytics/R/PRA/GARCH-25-08")

data<-read.csv("iex.csv", header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)

#Train
df.train<-df[1:160,]
#df.train
#df.test<-df[697:720]
#df.test

##ARIMA
fit <- arima(df$Price, order=c(2,0,1), seasonal = list(order = c(2, 1, 0), period=5))
#fit <- arima(df$Price, order=c(2,0,0), seasonal = list(order = c(0, 1, 1), period=24))
#fit <- arima(df$USD, order=c(1,0,0), seasonal = list(order = c(1, 1, 0), period=5))
summary(fit)
windows()
tsdiag(fit)


#ARIMA Forecasting
fit.forecast<-forecast(fit,h=24)
fit.forecast$mean
plot(fit.forecast, type="l")
lines(fit.forecast$mean,col = "green")
plot(fit$residuals, col="green")


#Normality Test
jarque.bera.test(fit$residuals)

#GARCH ESTIMATION
#archTest
archTest(fit$residuals, lag=30) #long-term
archTest(fit$residuals, lag=10) #medium-term
archTest(fit$residuals, lag=5) #short-term

#GARCH Specification
ug_spec<-ugarchspec()

ug_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                            submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
                      mean.model = list(armaOrder=c(0,0), include.mean = FALSE), 
                      distribution.model ="ged")

ug_spec

#Fit GARCH models

ug_fit<-ugarchfit(spec=ug_spec, data=fit$residuals)
ug_fit

#What slots are available for @model in ug_fit
ug_fit@model
names(ug_fit@model)

#What slots are available for @fit in ug_fit
ug_fit@fit
names(ug_fit@fit)
ug_fit@fit$coef;   #coeff

#Estimated conditional variance
ug_var<-ug_fit@fit$var; 

#ARIMA RESIDUALS/ACTUAL variance
ug_res2<-(fit$residuals)^2                 

#residual square
#ug_res2<-(ug_fit@fit$residuals)^2         #GARCH RESIDUALS AFTER ESTIMATION        


#Difference between ug_var and ug_res2 determines the accruacy of GARCH model
#Plot the Squared Residuals and the Estimated Conditional Variance 

##Estimated Variance
plot (ug_var,type="l")
lines(ug_var,col = "orange")

##Actual Variance
plot(ug_res2,typ="l")
lines(ug_res2,col="green")

##GARCH Model Forecasting: res and sE(res)
ug_fore<-ugarchforecast(ug_fit,n.ahead=24)

ug_fore@forecast$sigmaFor     # square root of forecast residual square
ug_fore@forecast$seriesFor    # arima part, for mean = false, it should be zero
#names(ug_fore@model)
#names(ug_fore@forecast)

ug_STER<-ug_fore@forecast$sigmaFor;
plot(ug_STER,type="l")
lines (ug_STER,col="green")

#Last 20 obs
ug_var_t <- c(tail(ug_var,10),rep(NA,10))
#ug_var_t
ug_res2_t <- c(tail(ug_res2,10),rep(NA,10))
#ug_res2_t
ug_fore2 <-c(rep(NA,10),(ug_fore@forecast$sigmaFor)^2)
#ug_fore2

plot (ug_res2_t,type="l")     #Actual Variance
lines (ug_fore2,col="orange") #Out-of sample Forecast Variance
lines (ug_var_t,col="green")  #Estimated Variance

##MAPE
#MAPE<-mean((abs(ug_res2_t-ug_fore2)/ug_res2)*100)
#MAPE



