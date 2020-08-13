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

##Identification Stage
#Plots: Graphs, ACF & PACF and Unit Root Tests

###Reliance Data
data<-read.csv(file.choose(),header=TRUE)
df<-data
nrow(df)
ncol(df)
head(df)
View(df)
#Plots: Graphs, ACF and PACF-------
##Identification: TREND: Level
windows()
tsdisplay(df$Close, lag=50)   #Graph, ACF, PACF

##Unit Root Tests: Trend -----------
k = trunc(length(df$Close)-1)^(1/3)  #k= number of lags
k

adf.test(df$Close, alternative="stationary", k=6)  #Trend Non-stationarity
adf.test(df$Close, alternative="stationary", k=12)

##Identification: First Difference/Trend Differencing

tsdisplay(diff(df$Close,1), lag=50)   #Graph, ACF, PACF
k = trunc((length(diff(df$Close,1))-1)^(1/3))
k
adf.test(diff(df$Close,1), alternative="stationary", k=6)  
adf.test(diff(df$Close,5), alternative="stationary", k=6)  

##Identification: SEASONAL: Seasonal Differencing
tsdisplay(diff(df$Close,5), lag=50)   #Graph, ACF, PACF

##Estimation
fit1 <- Arima(df$Close, order=c(1,0,0), seasonal = list(order = c(1, 1, 1), period=5))
fit1
summary(fit1)
fit2 <- Arima(df$Close, order=c(1,0,1), seasonal = list(order = c(4, 1, 0), period=5))
fit2
summary(fit2)

fit3 <- Arima(df$Close, order=c(0,1,0), seasonal = list(order = c(3, 1, 0), period=5))
fit3
summary(fit3)

##Diagnostics
tsdisplay(res)
Box.test(residuals(fit1), lag=5, fitdf=3, type="Ljung")
b2<-tsdiag(fit1)
b2

tsdisplay(res)
Box.test(residuals(fit2), lag=10, fitdf=6, type="Ljung")
b2<-tsdiag(fit2)
b2
