## install.packages("tseries")
#install.packages("forecast")
library(forecast)
library(tseries)

myFile <- file.choose()
data1=read.csv (myFile,header=TRUE)

#Plots & Correlogram
tsdisplay(data1$Close, lag=50)

#Unit root test
#k is where we find min variance after running various length lags
#for Close prices
k = trunc((length(data1$Close)-1)^(1/3));k
adf.test(na.omit(data1$Close), alternative="stationary", k=4)

#trend diff
tsdisplay(diff(data1$Close,1),lag=50)

#seasonal diff
tsdisplay(diff(data1$Close,5),lag=50)

fit1<-arima(data1$Close, order=c(1,0,1),  seasonal = list(order = c(1, 1, 1), period = 5))
fit1
summary(fit1)

fit2<-arima(data1$Close, order=c(1,0,0),  seasonal = list(order = c(0, 1, 1), period = 5))
fit2
summary(fit2)#coming best

##Diagnostics
res <- residuals(fit2)
tsdisplay(res)
Box.test(res, lag=4, fitdf=2, type="Ljung")#box-ljung test
Box.test(res, lag=10, fitdf=2, type="Ljung")
Box.test(res, lag=16, fitdf=2, type="Ljung")
b2<-tsdiag(fit2)
b2

hold <- window(ts(data1$Close), start=100)
no_holdout <- arima(ts(data1$Close[-c(101:115)]), order=c(1,0,0),  seasonal = list(order = c(0, 1, 1), period = 5))
forecast_no_holdout <- forecast(no_holdout,h=15)
summary(forecast_no_holdout)

plot(forecast_no_holdout, main=" ")
lines(ts(data1$Close))
summary(forecast_no_holdout)
summary(m4)

library(TSA)
covariate_x<-data.frame(data1$MACD,data1$sentiment)
m3<-arimax(data1$Close, order=c(1,0,0),  seasonal = list(order = c(0, 1, 1), period = 5),
           xtransf = covariate_x, xreg=covariate_x,method = c("ML"))

m3
summary(m3)
Box.test(residuals(m3), lag=4, fitdf=4, type="Ljung")
Box.test(residuals(m3), lag=5, fitdf=4, type="Ljung")
Box.test(residuals(m3), lag=9, fitdf=4, type="Ljung")
Box.test(residuals(m3), lag=15, fitdf=4, type="Ljung")

covariate_x<-data.frame(data1$MACD,data1$lg_senti)
m3<-arimax(data1$Close, order=c(1,0,0),  seasonal = list(order = c(0, 1, 1), period = 5),
           xtransf = covariate_x, xreg=covariate_x,method = c("ML"))
m3
summary(m3)