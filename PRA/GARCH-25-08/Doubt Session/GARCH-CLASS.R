rm(list=ls())
cat("\014")
dev.off()

install.packages("rugarch")
install.packages("MTS")
install.packages("forecast")
install.packages("tseries")

library (rugarch)
library (MTS)
library(tseries)
library(forecast)

#Import Data
options (scipen=99999)

data<-read.csv(file.choose(), header=TRUE)
df<-data [49:720,]
nrow(df)
ncol(df)
head(df)
View(df)

covariate_xmatrix <- matrix(c(df$SDPriceLS1),nrow=672,ncol=1)
covariate_xmatrix

#Train
#df.train<-df[7500:8806,]
#df.train
#df.test<-df[697:720]
#df.test

##ARIMA
fit <- arima(df.train$Euro, order=c(2,0,1), seasonal = list(order = c(2, 1, 0), period=5))
fit <- arima(df$Price, order=c(2,0,0), seasonal = list(order = c(1, 1,0), period=24))
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
#ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
#                                 submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
#           mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE, 
#                             archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE), 
#           distribution.model = "norm", start.pars = list(), fixed.pars = list(), ...)

ug_spec<-ugarchspec()

ug_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1), 
                                            submodel = NULL, external.regressors = NULL, variance.targeting = FALSE), 
                      mean.model = list(armaOrder=c(1,1), include.mean = TRUE, external.regressors = covariate_xmatrix), 
                      distribution.model ="snorm")

ug_spec

#Fit GARCH models

ug_fit<-ugarchfit(spec=ug_spec, data=df$SDPrice)
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

#names(ug_fore@model)
#names(ug_fore@forecast)

ug_STER<-ug_fore@forecast$sigmaFor;
plot(ug_STER,type="l")
lines (ug_STER,col="green")

#Last 20 obs
ug_var_t <- c(tail(ug_var,20),rep(NA,10))
#ug_var_t
ug_res2_t <- c(tail(ug_res2,20),rep(NA,10))
#ug_res2_t
ug_fore2 <-c(rep(NA,10),(ug_fore@forecast$sigmaFor)^2)
#ug_fore2

plot (ug_res2_t,type="l")     #Actual Variance
lines (ug_fore2,col="orange") #Out-of sample Forecast Variance
lines (ug_var_t,col="green")  #Estimated Variance

##MAPE
#MAPE<-mean((abs(ug_res2_t-ug_fore2)/ug_res2)*100)
#MAPE



