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

library(tseries)
library(forecast)
library(plm)
library(Formula)
library(tcltk)
library(uroot)
library(pdR)
library(stats)
options (scipen=99999)


###US Electricity Sales
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
windows()
tsdisplay(df$Sales, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$Sales)-1)^(1/3)  #k= number of lags
k

adf.test(df$Sales, alternative="stationary", k=7)  #Trend Non-stationarity
adf.test(df$Sales, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

#tsdisplay(diff(df$Sales,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$Sales,1))-1)^(1/3))
k
#adf.test(diff(df$Sales,1), alternative="stationary", k=6)  
adf.test(diff(df$Sales,12), alternative="stationary", k=12)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(diff(df$Sales,12),1), lag=50)   #Graph, ACF, PACF

##Estimation
fit1 <- Arima(df$Sales, order=c(1,0,0), seasonal = list(order = c(2, 1, 3), period=12))
fit1 <- Arima(df$Sales, order=c(1,0,0), seasonal = list(order = c(1, 1, 1), period=12))
fit1 <- Arima(df$Sales, order=c(1,0,0), seasonal = list(order = c(0, 1, 2), period=12))
fit1 <- Arima(df$Sales, order=c(1,0,0), seasonal = list(order = c(2, 1, 0), period=12))
fit1
summary(fit1)
fit2 <- Arima(df$searchvolume, order=c(3,0,1), seasonal = list(order = c(0, 1, 1), period=4))
fit2
summary(fit2)

##Diagnostics
tsdisplay(residuals(fit1))
Box.test(residuals(fit1), lag=5, fitdf=6, type="Ljung") # fitdf = no of parameters in model
Box.test(residuals(fit1), lag=10, fitdf=6, type="Ljung")
Box.test(residuals(fit1), lag=20, fitdf=6, type="Ljung")
windows()
b2<-tsdiag(fit1)


##Simle Forecast
fcast.fit1 <- forecast(fit1, h=30)
fcast.fit1
plot(fcast.fit1)

##Out-of-Sample Forecast
hold <- window(ts(df$Sales), start=277); 
fit1_no_holdout <- Arima(ts(df$Sales[-c(277:288)]), order=c(1,1,1),  seasonal = list(order = c(1, 1, 1), period = 12))
fit1_fcast_no_holdout <- forecast(fit1_no_holdout,h=12)
plot(fit1_fcast_no_holdout, main=" ")
lines(ts(df$Sales))
summary(fit1_fcast_no_holdout)

##Out-of-sample forecast accuracy
fcst_hold<-((abs(hold-fit1_fcast_no_holdout$mean)/hold))*100
mean(fcst_hold)

##auto.arima
fit1.auto<-auto.arima(df$Sales, d=NA, D=NA, seasonal = TRUE)
fit1.auto
summary(fit1.auto)

##Diagnostics
res <- residuals(fit1.auto)
tsdisplay(res)
Box.test(res, lag=16, fitdf=5, type="Ljung")
windows()
b1<-tsdiag(fit1.auto)
b1

fcast.auto <- forecast(fit1.auto, h=30)
plot(fcast.auto)

