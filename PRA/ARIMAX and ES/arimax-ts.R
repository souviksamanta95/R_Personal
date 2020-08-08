## Time Series ARIMA and MSARIMA Models in R: Box Jenkin's Methodology
rm(list=ls())
cat("\014")

install.packages("tseries")
install.packages("forecast")
install.packages("plm")
install.packages("Formula")
install.packages("tcltk")
install.packages("uroot")
install.packages("pdR")
install.packages("stats")
install.packages("TSA")

library(tseries)
library(forecast)
library(plm)
library(Formula)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
library(TSA)
options (scipen=99999)

data=read.csv("C:/Users/kakali/Desktop/PA/ARIMA-X/ts.csv")# Read the file
df<-data #ts
View(df)
#hold <- window(ts(df6), start=76)
#no_hold<-window(ts(df6, start=1, end=75))
#OOS<-data.frame(hold)
#IS<-data.frame(no_hold)


#Plots: Graphs, ACF and PACF-------
tsdisplay(df$TS, lag=50)

k = trunc((length(df$TS)-1)^(1/3))
k
#ADF TEST
adf.test(df$TS, alternative="stationary", k=4)

##Plots: Graphs, ACF and PACF: Trend and Seasonal Differencing
#tsdisplay(diff(df$TS,1),lag=50)

##adf.test
#adf.test(diff((df$TS),1), alternative="stationary", k=5)


##Plot the seasonal differenced series-----------
tsdisplay(diff(df$TS,12),lag=50)

##adf Test
#adf.test(diff((df$TS),12), alternative="stationary", k=4)

##Plot the trend and seasonal differenced series-----------
tsdisplay(diff(diff(df$TS,12),1),lag=50)
adf.test(diff(diff(df$TS,12),1), alternative="stationary", k=15)

##Estimation: ARIMA*MSARIMA Modelling
##After TREND Differencing
m1 <-arima(df$TS, order=c(1,1,1),  seasonal = list(order = c(0, 0, 1), period = 12))
fit_trend
summary(fit_trend)
Box.test(residuals(fit_trend), lag=10, fitdf=2, type="Ljung")

##After Seasonal Differencing
fit_seasonal<-arima(df6$TS, order=c(1,0,0),  seasonal = list(order = c(1, 1,0), period = 12))
fit_seasonal
summary(fit_seasonal)
Box.test(residuals(fit_seasonal), lag=6, fitdf=2, type="Ljung")

##After Trend and Seasonal Differencing
fit_t_s<-arima(df6$TS, order=c(0,1,1),  seasonal = list(order = c(0, 1, 1), period = 12))
fit_t_s
summary(fit_t_s)
Box.test(residuals(fit_t_s), lag=10, fitdf=2, type="Ljung")

##Arimax Estimation: ARIMA+covariate_X
covariate_x<-data.frame(df6$agri_cdt, df6$WPIT, df6$CPIAL, df6$IIP, df6$RainDev)
fit.arimax<-arimax(df6$TS, order=c(1,0,0),  seasonal = list(order = c(1, 1,0), period = 12),
                    xtransf = covariate_x, xreg=covariate_x,method = c("ML"))
coefficients(fit.arimax)
summary(fit.arimax)
residuals(fit.arimax)
Box.test(residuals(fit.arimax), lag=18, fitdf=3, type="Ljung")


##Simle Forecast
fcast.fit_trend <- forecast(fit_trend, h=12)
fcast.fit_trend
summary(fcast.fit_trend)
plot(fcast.fit_trend)

fcast.fit_seasonal <- forecast(fit_seasonal, h=12)
fcast.fit_seasonal
summary(fcast.fit_seasonal)
plot(fcast.fit_seasonal)

##

fcast.fit_arimax <- forecast(fit.arimax, n.ahead=12,newxreg=df[76:length(df)])
fcast.fit_arimax
summary(fcast.fit_arimax)
plot(fcast.fit_arimax)


##Out-of-Sample Forecast
fit_no_hold <- arimax(df6$TS[-c(76,86)], order=c(1,0,0),  seasonal = list(order = c(1, 1,0), period = 12),
                         xtransf =df6$agri_cdt, xreg=df6$agri_cdt,method = c("ML"))
summary(fit_no_hold)
fit_fcast_no_hold <- forecast(fit_no_hold,h=12)
f1<-fit_fcast_no_holdout;f1
plot(fit1_fcast_no_holdout, main=" ")
lines(ts(df6$fit_no_hold))
summary(fit1_fcast_no_hold)

##auto.arima
fit1.auto<-auto.arima(df1$FTD, seasonal = TRUE)
fit1.auto
summary(fit1.auto)

##Diagnostics
res <- residuals(fit1.auto)
tsdisplay(res)
Box.test(res, lag=16, fitdf=5, type="Ljung")
b1<-tsdiag(fit1.auto)
b1

##Simple Forecast with auto.arima
fcast.auto <- forecast(fit1.auto, h=30)
plot(fcast.auto)

##out-of-sample with auto-arima
hold <- window(ts(df1$FTD), start=100)
df1<-cbind(df1,hold)
fit1_auto_no_holdout <- auto.arima(ts(df1$FTD[-c(100:113)]), seasonal=TRUE)
fit1_auto_fcast_no_holdout <- forecast(fit1_auto_no_holdout,h=12)
fit1_auto_fcast_no_holdout
df1<-cbind(df1,fit1_auto_fcast_no_holdout)
plot(fit1_auto_fcast_no_holdout, main=" ")
lines(ts(df1$FTD))
summary(fit1_auto_fcast_no_holdout)





